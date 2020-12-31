{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Queries from local clients to the node.
--
module Cardano.Api.Query (

    -- * Queries
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,

    -- TODO: Move
    StakeDistribution(..),
  ) where

import           Data.Bifunctor (bimap)
import           Data.List (group)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.SOP.Strict (SListI)
import           Prelude

import           Ouroboros.Network.Block (Serialised)
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus

import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Cardano.Ledger.Era as Ledger
import qualified Shelley.Spec.Ledger.BaseTypes as Ledger
import qualified Shelley.Spec.Ledger.Credential as Ledger
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.LedgerState as Ledger

import           Cardano.Api.Address
import           Cardano.Api.Block (ChainPoint, fromConsensusPoint)
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.KeysShelley
import           Cardano.Api.Modes (ConsensusBlockForEra, ConsensusBlockForMode,
                     ConsensusMode (CardanoMode), ConsensusModeIsMultiEra (..), EraInMode (..),
                     anyEraInModeToAnyEra, fromConsensusEraIndex)
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Value
import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Shelley


-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode mode result where

     QueryCurrentEra :: ConsensusModeIsMultiEra mode
                     -> QueryInMode mode AnyCardanoEra

     QueryInEra      :: EraInMode era mode
                     -> QueryInEra era result
                     -> QueryInMode mode (Either EraMismatch result)

--TODO: add support for these
--     QueryEraStart   :: ConsensusModeIsMultiEra mode
--                     -> EraInMode era mode
--                     -> QueryInMode mode (Maybe EraStart)

--     QueryEraHistory :: QueryInMode mode EraHistory

deriving instance Show (QueryInMode mode result)


data QueryInEra era result where
     QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra result
                            -> QueryInEra era result

deriving instance Show (QueryInEra era result)

data QueryInShelleyBasedEra result where
     QueryChainPoint
       :: QueryInShelleyBasedEra ChainPoint

     QueryEpoch
       :: QueryInShelleyBasedEra EpochNo

--TODO: add support for these
--     QueryGenesisParameters
--       :: QueryInShelleyBasedEra GenesisParameters

     QueryProtocolParameters
       :: QueryInShelleyBasedEra ProtocolParameters

--     QueryProtocolParametersUpdate
--       :: QueryInShelleyBasedEra ProtocolParametersUpdate

     QueryStakeDistribution
       :: QueryInShelleyBasedEra StakeDistribution

--     QueryUTxO
--       :: Maybe (Set AddressAny)
--       -> QueryInShelleyBasedEra UTxO

     QueryStakeAddresses
       :: Set StakeAddress
       -> QueryInShelleyBasedEra (Map StakeAddress Lovelace,
                                  Map StakeAddress PoolId)

--     QueryPoolRanking
--       ::
--       -> QueryInShelleyBasedEra

     QueryLedgerState
       :: QueryInShelleyBasedEra LedgerState

--     QueryProtocolState
--       :: QueryInShelleyBasedEra ProtocolState

deriving instance Show (QueryInShelleyBasedEra result)

-- ----------------------------------------------------------------------------
-- Wrapper types used in queries
--

--TODO: provide appropriate instances for these types as needed, e.g. JSON

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show


-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery :: forall mode block result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode result
                 -> Some (Consensus.Query block)
toConsensusQuery (QueryCurrentEra CardanoModeIsMultiEra) =
    Some (Consensus.QueryHardFork Consensus.GetCurrentEra)

toConsensusQuery (QueryInEra ByronEraInByronMode QueryByronUpdateState) =
    Some (Consensus.DegenQuery Consensus.GetUpdateInterfaceState)

toConsensusQuery (QueryInEra ByronEraInCardanoMode QueryByronUpdateState) =
    Some (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState)

toConsensusQuery (QueryInEra erainmode (QueryInShelleyBasedEra era q)) =
    case erainmode of
      ByronEraInByronMode     -> case era of {}
      ShelleyEraInShelleyMode -> toConsensusQueryShelleyBased erainmode q
      ByronEraInCardanoMode   -> case era of {}
      ShelleyEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      AllegraEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      MaryEraInCardanoMode    -> toConsensusQueryShelleyBased erainmode q


toConsensusQueryShelleyBased
  :: forall era mode ledgerera block xs result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock ledgerera
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => Ledger.Crypto ledgerera ~ StandardCrypto
  => EraInMode era mode
  -> QueryInShelleyBasedEra result
  -> Some (Consensus.Query block)
toConsensusQueryShelleyBased erainmode QueryChainPoint =
    Some (consensusQueryInEraInMode erainmode Consensus.GetLedgerTip)

toConsensusQueryShelleyBased erainmode QueryEpoch =
    Some (consensusQueryInEraInMode erainmode Consensus.GetEpochNo)

toConsensusQueryShelleyBased erainmode QueryProtocolParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetCurrentPParams)

toConsensusQueryShelleyBased erainmode QueryStakeDistribution =
    Some (consensusQueryInEraInMode erainmode Consensus.GetStakeDistribution)

toConsensusQueryShelleyBased erainmode (QueryStakeAddresses stakeAddresses) = do
    let stakeCredentials = Set.map (\(StakeAddress _ cred) -> cred) stakeAddresses
    Some (consensusQueryInEraInMode erainmode $ Consensus.GetFilteredDelegationsAndRewardAccounts stakeCredentials)

toConsensusQueryShelleyBased erainmode QueryLedgerState =
    Some (consensusQueryInEraInMode erainmode $ Consensus.GetCBOR Consensus.DebugNewEpochState)

consensusQueryInEraInMode
  :: forall era mode erablock modeblock result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => ConsensusBlockForMode mode ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => EraInMode era mode
  -> Consensus.Query erablock  result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode ByronEraInByronMode     = Consensus.DegenQuery
consensusQueryInEraInMode ShelleyEraInShelleyMode = Consensus.DegenQuery
consensusQueryInEraInMode ByronEraInCardanoMode   = Consensus.QueryIfCurrentByron
consensusQueryInEraInMode ShelleyEraInCardanoMode = Consensus.QueryIfCurrentShelley
consensusQueryInEraInMode AllegraEraInCardanoMode = Consensus.QueryIfCurrentAllegra
consensusQueryInEraInMode MaryEraInCardanoMode    = Consensus.QueryIfCurrentMary


-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult :: forall mode block result result'.
                            ConsensusBlockForMode mode ~ block
                         => QueryInMode mode result
                         -> Consensus.Query block result'
                         -> result'
                         -> result
fromConsensusQueryResult (QueryCurrentEra CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.QueryHardFork Consensus.GetCurrentEra ->
        anyEraInModeToAnyEra (fromConsensusEraIndex CardanoMode r')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     QueryByronUpdateState) q' r' =
    case (q', r') of
      (Consensus.DegenQuery Consensus.GetUpdateInterfaceState,
       Consensus.DegenQueryResult r'') ->
        Right (ByronUpdateState r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     QueryByronUpdateState) q' r' =
    case q' of
      Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState ->
        bimap fromConsensusEraMismatch ByronUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInShelleyMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case (q', r') of
      (Consensus.DegenQuery q'', Consensus.DegenQueryResult r'') ->
        Right (fromConsensusQueryResultShelleyBased q q'' r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentShelley q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AllegraEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentAllegra q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra MaryEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentMary q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased q q'')
              r'
      _ -> fromConsensusQueryResultMismatch


fromConsensusQueryResultShelleyBased
  :: forall ledgerera result result'.
     Consensus.ShelleyBasedEra ledgerera
  => Ledger.Crypto ledgerera ~ StandardCrypto
  => QueryInShelleyBasedEra result
  -> Consensus.Query (Consensus.ShelleyBlock ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultShelleyBased QueryChainPoint q' point =
    case q' of
      Consensus.GetLedgerTip -> fromConsensusPoint point
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased QueryEpoch q' epoch =
    case q' of
      Consensus.GetEpochNo -> epoch
      _                    -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased QueryProtocolParameters q' pParams =
    case q' of
      Consensus.GetCurrentPParams -> fromShelleyBasedParams pParams
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased QueryStakeDistribution q' stakeDist =
    case q' of
      Consensus.GetStakeDistribution -> StakeDistribution stakeDist
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased (QueryStakeAddresses stakeCreds) q' result  =
    case q' of
      Consensus.GetFilteredDelegationsAndRewardAccounts _ -> fromDelegsAndRewards result stakeCreds
      _                                                   -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased QueryLedgerState q' ledgerStateCBOR =
    case q' of
      Consensus.GetCBOR Consensus.DebugNewEpochState -> LedgerState ledgerStateCBOR
      _                                              -> fromConsensusQueryResultMismatch

data LedgerState where
  LedgerState :: Serialised (Ledger.NewEpochState ledgercrypto) -> LedgerState

data StakeDistribution where
  StakeDistribution :: Shelley.PoolDistr ledgercrypto -> StakeDistribution

fromDelegsAndRewards
  :: ( Map (Ledger.Credential Ledger.Staking StandardCrypto) (Ledger.KeyHash Ledger.StakePool StandardCrypto)
     , Ledger.RewardAccounts StandardCrypto
     )
  -> Set StakeAddress
  -> (Map StakeAddress Lovelace, Map StakeAddress PoolId)
fromDelegsAndRewards (delegMap, rewardAccts) submitedStakeAddrs =
  let networkId = getNetworkId submitedStakeAddrs
      rewardsMap = Map.fromList . map (\(cred, coin) -> (StakeAddress networkId cred, fromShelleyLovelace coin)) $ Map.toList rewardAccts
      delegationMap = Map.fromList . map (\(cred, poolId) -> (StakeAddress networkId cred, StakePoolKeyHash poolId)) $ Map.toList delegMap
  in (rewardsMap, delegationMap)

getNetworkId :: Set StakeAddress -> Ledger.Network
getNetworkId stakeAddrs = do
  let networkIds = Set.toList $ Set.map (\(StakeAddress nw _) -> nw) stakeAddrs
  case networkIds of
    nId:_ -> case length $ group networkIds of
               1 -> nId
               _ -> error $ "Multiple network ids in stake addresses:" <> show stakeAddrs
    [] -> error "No stake address submitted with query"

-- | This should /only/ happen if we messed up the mapping in 'toConsensusQuery'
-- and 'fromConsensusQueryResult' so they are inconsistent with each other.
--
-- If we do encounter this error it means that 'toConsensusQuery' maps a
-- API query constructor to a certain consensus query constructor but that
-- 'fromConsensusQueryResult' apparently expects a different pairing.
--
-- For example, imagine if 'toConsensusQuery would (incorrectly) map
-- 'QueryChainPoint' to 'Consensus.GetEpochNo' but 'fromConsensusQueryResult'
-- (correctly) expected to find 'Consensus.GetLedgerTip'. This mismatch would
-- trigger this error.
--
-- Such mismatches should be preventable with an appropriate property test.
--
fromConsensusQueryResultMismatch :: a
fromConsensusQueryResultMismatch =
    error "fromConsensusQueryResult: internal query mismatch"


fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch

