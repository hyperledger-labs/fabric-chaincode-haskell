{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedLists   #-}

module Shim where

import Peer.ChaincodeShim as Pb
import Peer.Chaincode as Pb
import Peer.ProposalResponse as Pb
import Network.GRPC.HighLevel.Generated
import qualified Network.GRPC.LowLevel.Client as Client
import Proto3.Suite as Suite
import Proto3.Wire.Encode as Wire
import Proto3.Wire.Decode as Wire
import Proto3.Wire
import Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 as BC
import Data.Text.Encoding as TSE
import Data.Text.Lazy

import Debug.Trace

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "localhost"
                            , clientServerPort = 7052
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            , clientAuthority = Nothing
                            }
regPayload :: Pb.ChaincodeID
regPayload = Pb.ChaincodeID {
    chaincodeIDName = "mycc:v0",
    chaincodeIDPath = "chaincodedev/chaincode/chaincode_example02/go",
    chaincodeIDVersion = "v0"
}

initPayload :: Pb.Response
initPayload = Pb.Response{
    responseStatus = 200,
    responseMessage = "Successfully initialised",
    responsePayload = TSE.encodeUtf8 "40"
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

initCompletedMessage :: Text -> Text -> Pb.Response -> ChaincodeMessage
initCompletedMessage txID chanID res = ChaincodeMessage{
    chaincodeMessageType = Enumerated $ Right ChaincodeMessage_TypeCOMPLETED,
    chaincodeMessageTimestamp = Nothing,
    chaincodeMessagePayload = LBS.toStrict $ Wire.toLazyByteString $ encodeMessage (FieldNumber 2) res,
    chaincodeMessageTxid = txID,
    chaincodeMessageProposal = Nothing,
    chaincodeMessageChaincodeEvent = Nothing,
    chaincodeMessageChannelId = chanID
}

start :: (Int -> Pb.Response) -> IO ()
start initFn = withGRPCClient clientConfig $ grpcRunner initFn

grpcRunner :: (Int -> Pb.Response) -> Client.Client -> IO ()
grpcRunner initFn client = do
        -- contains chaincodeSupportRegister function
        Pb.ChaincodeSupport{..} <- chaincodeSupportClient client


        -- NOTE: This 2000 seconds is a hack till this gets resolved https://github.com/awakesecurity/gRPC-haskell/issues/100
        _ <- chaincodeSupportRegister $ ClientBiDiRequest 2000 [] $ biDiRequestFn initFn

        print "testing"



-- biDiRequestFn :: ClientCall -> MetadataMap -> StreamRecv ChaincodeMessage ->
--          StreamSend ChaincodeMessage -> WritesDone -> IO ()
biDiRequestFn initFn _call _mmap recv send _done = do
    e <- send regMessage :: IO (Either GRPCIOError ())
    case e of
        Left err -> error ("Error while streaming: " ++ show err)
        Right _ -> trace "okie dokey" pure ()
    chatWithPeer recv send initFn

chatWithPeer recv send initFn = do
    res <- recv
    case res of
        Left err -> error ("OH MY GOD: " ++ show err)
        Right (Just message) -> handler message send initFn
        Right Nothing -> print "I got no message... wtf"
    chatWithPeer recv send initFn

handler message send initFn = case message of
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeREGISTERED)} ->  print "YAY REGGED"
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeREADY)} ->  print "YAY READY"
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeINIT)} ->  trace "YAY INIT" $ handleInit message send initFn
    s ->  print ("Goodie:" ++ show s)


handleInit mes send initFn = let
    eErrInput = Suite.fromByteString (chaincodeMessagePayload mes) :: Either ParseError Pb.ChaincodeInput
    in
    case eErrInput of
        Left err -> error (show err)
        Right Pb.ChaincodeInput{chaincodeInputArgs= args} -> do
            e <- send (initCompletedMessage (chaincodeMessageTxid mes) (chaincodeMessageChannelId mes) (initFn 1)) :: IO (Either GRPCIOError ())
            case e of
                Left err -> error ("Error while streaming: " ++ show err)
                Right _ -> trace "okie dokey init" pure ()
