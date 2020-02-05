{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedLists   #-}

module Main where

import Peer.ChaincodeShim as Shim
import qualified Peer.Chaincode as Shim
import Network.GRPC.HighLevel.Generated
import qualified Network.GRPC.LowLevel.Client as Client
import Proto3.Suite
import Proto3.Wire.Encode as Wire
import Proto3.Wire
import Data.ByteString.Lazy as LBS

import Debug.Trace

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "localhost"
                            , clientServerPort = 7052
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            , clientAuthority = Nothing
                            }
regPayload :: Shim.ChaincodeID
regPayload = Shim.ChaincodeID {
    chaincodeIDName = "mycc",
    chaincodeIDPath = "testpath",
    chaincodeIDVersion = "1"
}

regMessage :: ChaincodeMessage
regMessage = ChaincodeMessage{
    chaincodeMessageType = Enumerated $ Right ChaincodeMessage_TypeREGISTER,
    chaincodeMessageTimestamp = Nothing,
    chaincodeMessagePayload = LBS.toStrict $ Wire.toLazyByteString $ encodeMessage (FieldNumber 1) regPayload,
    chaincodeMessageTxid = "mytxid",
    chaincodeMessageProposal = Nothing,
    chaincodeMessageChaincodeEvent = Nothing,
    chaincodeMessageChannelId = "myc"
}

main :: IO ()
main = withGRPCClient clientConfig grpcRunner

grpcRunner :: Client.Client -> IO ()
grpcRunner client = do
        -- contains chaincodeSupportRegister function
        Shim.ChaincodeSupport{..} <- chaincodeSupportClient client


        _ <- chaincodeSupportRegister $ ClientBiDiRequest 5 [] biDiRequestFn

        print "testing"



-- biDiRequestFn :: ClientCall -> MetadataMap -> StreamRecv ChaincodeMessage ->
--          StreamSend ChaincodeMessage -> WritesDone -> IO ()
biDiRequestFn _call _mmap _recv send _ = do
    e <- send regMessage :: IO (Either GRPCIOError ())
    case e of
        Left err -> error ("Error while streaming: " ++ show err)
        Right _ -> trace "okie dokey" pure ()
