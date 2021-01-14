{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import           Numeric (showEFloat)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistMaybe, left,
                     newExceptT)

import           Cardano.Api
import           Cardano.Api.Byron
import qualified Cardano.Api.IPC as NewIPC
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Modes (AnyConsensusModeParams (..), toEraInMode)
import qualified Cardano.Api.Modes as Mode
import           Cardano.Api.Protocol
import           Cardano.Api.ProtocolParameters
import qualified Cardano.Api.Query as Query
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Mary.RenderValue (defaultRenderValueOptions, renderValue)
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))
import           Cardano.CLI.Types

import           Cardano.Binary (decodeFull)
import           Cardano.Crypto.Hash (hashToBytesAsHex)

import           Ouroboros.Consensus.Cardano.Block as Consensus (Either (..), EraMismatch (..),
                     Query (..))
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import           Ouroboros.Network.Block (Serialised (..), getTipPoint)

import qualified Shelley.Spec.Ledger.API.Protocol as Ledger
import           Shelley.Spec.Ledger.Scripts ()

import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
                     (AcquireFailure (..))

data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquireFailure
  | ShelleyQueryCmdByronEraDetected
  | ShelleyQueryCmdEraMismatch !EraMismatch
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyCardanoEra !AnyConsensusModeParams
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdAcquireFailure aqFail -> Text.pack $ show aqFail
    ShelleyQueryCmdByronEraDetected -> "This query cannot be used for the Byron era"
    ShelleyQueryCmdEraConsensusModeMismatch (AnyCardanoEra era) (AnyConsensusModeParams cModeParams) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cModeParams <>
      " Era: " <> show era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "An error mismatch occured. Specified query era: " <> queryEra <>
      "Current ledger era: " <> ledgerEra

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters era consensusModeParams network mOutFile ->
      runQueryProtocolParameters era consensusModeParams network mOutFile
    QueryTip consensusModeParams network mOutFile ->
      runQueryTip consensusModeParams network mOutFile
    QueryStakeDistribution era consensusModeParams network mOutFile ->
      runQueryStakeDistribution era consensusModeParams network mOutFile
    QueryStakeAddressInfo era consensusModeParams addr network mOutFile ->
      runQueryStakeAddressInfo era consensusModeParams addr network mOutFile
    QueryLedgerState era consensusModeParams network mOutFile ->
      runQueryLedgerState era consensusModeParams network mOutFile
    QueryProtocolState era protocol network mOutFile ->
      runQueryProtocolState era protocol network mOutFile
    QueryUTxO era protocol qFilter networkId mOutFile ->
      runQueryUTxO era protocol qFilter networkId mOutFile

runQueryProtocolParameters
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters anyEra@(AnyCardanoEra era) anyCmodeParams@(AnyConsensusModeParams cModeParams)
                           network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr
                           readEnvSocketPath
  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra anyCmodeParams) $ toEraInMode cModeParams era

  let localNodeConnInfo = NewIPC.LocalNodeConnectInfo cModeParams network sockPath
      qInMode = NewIPC.createQueryInMode eraInMode NewIPC.QueryProtocolParameters

  tip <- liftIO $ NewIPC.getLocalChainTip localNodeConnInfo
  res <- liftIO $ NewIPC.queryNodeLocalState localNodeConnInfo tip qInMode
  case res of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right ePparams ->
      case ePparams of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right pparams -> writeProtocolParameters mOutFile pparams

writeProtocolParameters
  :: Maybe OutputFile
  -> ProtocolParameters
  -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip (AnyConsensusModeParams cModeParams) network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    let localNodeConnInfo = NewIPC.LocalNodeConnectInfo cModeParams network sockPath

    tip <- liftIO $ NewIPC.getLocalChainTip localNodeConnInfo

    let output = case NewIPC.localConsensusMode localNodeConnInfo of
                   Mode.ByronMode   -> encodePretty tip
                   Mode.ShelleyMode -> encodePretty tip
                   Mode.CardanoMode -> encodePretty tip

    case mOutFile of
      Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath output
      Nothing                 -> liftIO $ LBS.putStrLn        output

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--

runQueryUTxO
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> QueryFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO anyEra@(AnyCardanoEra era) anyCmodeParams@(AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra anyCmodeParams)
                 $ toEraInMode cModeParams era
  let localNodeConnInfo = NewIPC.LocalNodeConnectInfo cModeParams network sockPath
      qInMode = NewIPC.createQueryInMode
                  eraInMode
                  (NewIPC.QueryUTxO $ maybeFiltered qfilter)

  tip <- liftIO $ NewIPC.getLocalChainTip localNodeConnInfo
  eUtxo <- liftIO $ NewIPC.queryNodeLocalState localNodeConnInfo tip qInMode
  case eUtxo of
    Left aF -> left $ ShelleyQueryCmdAcquireFailure aF
    Right eU -> case eU of
                  Left mismatch -> left $ ShelleyQueryCmdEraMismatch mismatch
                  Right utxo -> writeFilteredUTxOs mOutFile utxo
 where
  maybeFiltered :: QueryFilter -> Maybe (Set AddressAny)
  maybeFiltered (FilterByAddress as) = Just as
  maybeFiltered NoFilter = Nothing

runQueryLedgerState
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState anyEra@(AnyCardanoEra era) anyCmodeParams@(AnyConsensusModeParams cModeParams)
                    network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra anyCmodeParams)
                  $ toEraInMode cModeParams era
    let localNodeConnInfo = NewIPC.LocalNodeConnectInfo cModeParams network sockPath
        qInMode = NewIPC.createQueryInMode eraInMode NewIPC.QueryLedgerState

    tip <- liftIO $ NewIPC.getLocalChainTip localNodeConnInfo
    res <- liftIO $ NewIPC.queryNodeLocalState localNodeConnInfo tip qInMode
    case res of
      Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
      Right eStakeDist ->
        case eStakeDist of
          Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
          Right stakeDist -> writeLedgerState mOutFile stakeDist

runQueryProtocolState
  :: AnyCardanoEra
  -> Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState (AnyCardanoEra era) protocol network mOutFile
  | ShelleyBasedEra era' <- cardanoEraStyle era = do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    els <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo protocol network sockPath $
        queryLocalProtocolState era'
    case els of
      Right protocolState -> writeProtocolState mOutFile protocolState
      Left pbs -> do
        liftIO $ putTextLn "Version mismatch between node and consensus, so dumping this as generic CBOR."
        firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR pbs

    | otherwise = throwError (ShelleyQueryCmdLocalStateQueryError
                              ByronProtocolNotSupportedError)

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runQueryStakeAddressInfo
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo anyEra@(AnyCardanoEra era) anyCmodeParams@(AnyConsensusModeParams cModeParams)
                         (StakeAddress _ addr) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra anyCmodeParams)
                 $ toEraInMode cModeParams era

  let localNodeConnInfo = NewIPC.LocalNodeConnectInfo cModeParams network sockPath
      qInMode = NewIPC.createQueryInMode
                  eraInMode
                  (NewIPC.QueryStakeAddresses (Set.singleton $ fromShelleyStakeCredential addr) network)

  tip <- liftIO $ NewIPC.getLocalChainTip localNodeConnInfo
  res <- liftIO $ NewIPC.queryNodeLocalState localNodeConnInfo tip qInMode
  case res of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right eDelegsAndRwds ->
      case eDelegsAndRwds of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right delegsAndRewards -> writeStakeAddressInfo mOutFile $ DelegationsAndRewards delegsAndRewards


-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  | ShelleyProtocolEraMismatch
  -- ^ The Shelley protocol only supports the Shelley era.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ByronProtocolNotSupportedError ->
      "The attempted local state query does not support the Byron protocol."
    ShelleyProtocolEraMismatch ->
        "The Shelley protocol mode can only be used with the Shelley era, "
     <> "i.e. with --shelley-mode use --shelly-era flag"

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: Maybe OutputFile
                 -> Query.LedgerState era
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile (Query.LedgerState serLedgerState) =
  case mOutFile of
    Nothing -> liftIO . LBS.putStrLn $ unSerialised serLedgerState
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeProtocolState :: Maybe OutputFile
                   -> Ledger.ChainDepState StandardCrypto
                   -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile pstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty pstate)

writeFilteredUTxOs
  :: IsCardanoEra era
  => Maybe OutputFile
  -> Query.UTxO era
  -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
      case mOutFile of
          Nothing -> liftIO $ printFilteredUTxOs utxo
          Just (OutputFile fpath) ->
            handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: Query.UTxO era -> IO ()
printFilteredUTxOs (Query.UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Amount"

    printUtxo ::(TxIn, TxOut era) -> IO ()
    printUtxo (TxIn (TxId txhash) txin, TxOut _ value) =
      Text.putStrLn $
        mconcat
          [ Text.decodeLatin1 (hashToBytesAsHex txhash)
          , textShowN 6 txin
          , "        " <> printableValue value
          ]

    textShowN :: Show a => Int -> a -> Text
    textShowN len x =
      let str = show x
          slen = length str
      in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

    printableValue :: TxOutValue era -> Text
    printableValue (TxOutValue _ val) = renderValue defaultRenderValueOptions val
    printableValue (TxOutAdaOnly _ (Lovelace i)) = Text.pack $ show i


runQueryStakeDistribution
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution anyEra@(AnyCardanoEra era) anyCmodeParams@(AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra anyCmodeParams) $ toEraInMode cModeParams era

  let localNodeConnInfo = NewIPC.LocalNodeConnectInfo cModeParams network sockPath
      qInMode = NewIPC.createQueryInMode eraInMode NewIPC.QueryStakeDistribution

  tip <- liftIO $ NewIPC.getLocalChainTip localNodeConnInfo
  res <- liftIO $ NewIPC.queryNodeLocalState localNodeConnInfo tip qInMode
  case res of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right eStakeDist ->
      case eStakeDist of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right stakeDist ->  writeStakeDistribution mOutFile stakeDist

writeStakeDistribution
  :: Maybe OutputFile
  -> Map PoolId Rational
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) stakeDistrib =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakeDistrib)

writeStakeDistribution Nothing stakeDistrib =
  liftIO $ printStakeDistribution stakeDistrib


printStakeDistribution :: Map PoolId Rational -> IO ()
printStakeDistribution stakeDistrib = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  sequence_
    [ putStrLn $ showStakeDistr poolId stakeFraction
    | (poolId, stakeFraction) <- Map.toList stakeDistrib ]
 where
   title :: Text
   title =
     "                           PoolId                                 Stake frac"

   showStakeDistr :: PoolId
                  -> Rational
                  -- ^ Stake fraction
                  -> String
   showStakeDistr poolId stakeFraction =
     concat
       [ Text.unpack (serialiseToBech32 poolId)
       , "   "
       , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
       ]

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress Lovelace, Map StakeAddress PoolId)


mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
 [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
 | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
 ]


instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ mergeDelegsAndRewards delegsAndRwds
    where
      delegAndRwdToJson :: (StakeAddress, Maybe Lovelace, Maybe PoolId) -> Aeson.Value
      delegAndRwdToJson (addr, mRewards, mPoolId) =
        Aeson.object
          [ "address" .= serialiseAddress addr
          , "delegation" .= mPoolId
          , "rewardAccountBalance" .= mRewards
          ]

queryLocalProtocolState
  :: forall era ledgerera mode block.
     ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO
             (Either LByteString (Ledger.ChainDepState StandardCrypto))
queryLocalProtocolState era connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} | ShelleyBasedEraShelley <- era -> do
      tip <- liftIO $ getLocalTip connectInfo
      Consensus.DegenQueryResult result <-
        firstExceptT AcquireFailureError . newExceptT $
          queryNodeLocalState
            connectInfo
            ( getTipPoint tip
            , Consensus.DegenQuery $
                Consensus.GetCBOR Consensus.DebugChainDepState
                -- Get CBOR-in-CBOR version
            )
      return (decodeProtocolState result)

    ShelleyMode{} | otherwise -> throwError ShelleyProtocolEraMismatch

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip,
           queryIfCurrentEra era (Consensus.GetCBOR Consensus.DebugChainDepState))
                                  -- Get CBOR-in-CBOR version
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess ls -> return (decodeProtocolState ls)
  where
    -- If decode as a ChainDepState fails we return the ByteString so we can do a generic
    -- CBOR decode.
    decodeProtocolState (Serialised pbs) =
      first (const pbs) (decodeFull pbs)

-- -----------------------------------------------------------------------------
-- Era-generic helper functions
--

-- | Select the appropriate query constructor based on the era
-- 'QueryIfCurrentShelley', 'QueryIfCurrentAllegra' or 'QueryIfCurrentMary'.
--
--
queryIfCurrentEra :: ShelleyBasedEra era
                  -> Query (Consensus.ShelleyBlock (ShelleyLedgerEra era)) result
                  -> Consensus.CardanoQuery StandardCrypto
                       (Consensus.CardanoQueryResult StandardCrypto result)
queryIfCurrentEra ShelleyBasedEraShelley = Consensus.QueryIfCurrentShelley
queryIfCurrentEra ShelleyBasedEraAllegra = Consensus.QueryIfCurrentAllegra
queryIfCurrentEra ShelleyBasedEraMary    = Consensus.QueryIfCurrentMary

