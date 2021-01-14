module Main where

import Cardano.Api hiding (LocalNodeClientProtocols(..))
import Cardano.Api.IPC -- needed for now

--TODO: avoid name clashes and export this via Cardano.Api
import Ouroboros.Network.Protocol.ChainSync.Client


main :: IO ()
main = do
    connectToLocalNode
      connectInfo
      protocols

connectInfo :: LocalNodeConnectInfo
connectInfo =
    LocalNodeConnectInfo {
      localConsensusModeParams = CardanoModeParams 21600,
      localNodeNetworkId       = Mainnet,
      localNodeSocketPath      = "db/node.sock"
    }

protocols :: LocalNodeClientProtocolsInMode CardanoMode
protocols =
    LocalNodeClientProtocols {
      localChainSyncClient    = Nothing,
      localTxSubmissionClient = Nothing,
      localStateQueryClient   = Nothing
    }


chainSyncClient :: ChainSyncClient
                     (BlockInMode CardanoMode)
                     ChainPoint
                     ChainTip
                     IO ()
chainSyncClient =
    ChainSyncClient (pure clientStIdle)
  where
    clientStIdle :: ClientStIdle (BlockInMode CardanoMode)
                                 ChainPoint ChainTip IO ()
    clientStIdle =
      SendMsgRequestNext
        -- There's more to get immediately
        clientStNext

        -- The node is asking us to wait. This is because we reached the
        -- tip. We can certainly carry on here, but for this demo we are
        -- going to stop when we hit the current chain tip.
        (pure clientDone)

    clientStNext :: ClientStNext (BlockInMode CardanoMode)
                                 ChainPoint ChainTip IO ()
    clientStNext =
      ClientStNext {
        recvMsgRollForward = \blk tip ->
          ChainSyncClient $ do
            print blk
            pure (clientStIdle)

      , recvMsgRollBackward = \pt tip ->
          ChainSyncClient $ do
            print pt
            pure (clientStIdle)
      }

    -- We're still in the "Next" state here, but we've decided to stop
    -- as soon as we get the reply, no matter which reply.
    clientDone =
      ClientStNext {
        recvMsgRollForward  = \_ _ -> ChainSyncClient (pure (SendMsgDone ())),
        recvMsgRollBackward = \_ _ -> ChainSyncClient (pure (SendMsgDone ()))
      }

