{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedLists   #-}

module Shim where

import Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 as BC
import Data.Text.Encoding as TSE
import Data.Text.Lazy

import Network.GRPC.HighLevel.Generated
import qualified Network.GRPC.LowLevel.Client as Client
import Proto3.Suite as Suite
import Proto3.Wire.Encode as Wire
import Proto3.Wire.Decode as Wire
import Proto3.Wire

import Peer.ChaincodeShim as Pb
import Peer.Chaincode as Pb
import Peer.ProposalResponse as Pb

import Stub
import Interfaces

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

successPayload :: Pb.Response
successPayload = Pb.Response{
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

data ChaincodeStub = ChaincodeStub {
    initFn :: DefaultChaincodeStub -> Pb.Response,
    invokeFn :: DefaultChaincodeStub -> Pb.Response
}

stub = DefaultChaincodeStub Nothing Nothing Nothing "HIHINI" Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- TODO: start :: ChaincodeStub a => (a -> Pb.Response) -> IO ()
start :: ChaincodeStub -> IO ()
start chaincodeStub = withGRPCClient clientConfig $ grpcRunner chaincodeStub

grpcRunner :: ChaincodeStub -> Client.Client -> IO ()
grpcRunner chaincodeStub client = do
        -- contains chaincodeSupportRegister function
        Pb.ChaincodeSupport{..} <- chaincodeSupportClient client


        -- NOTE: This 2000 seconds is a hack till this gets resolved https://github.com/awakesecurity/gRPC-haskell/issues/100
        _ <- chaincodeSupportRegister $ ClientBiDiRequest 2000 [] $ biDiRequestFn chaincodeStub

        print "testing"



-- biDiRequestFn :: ClientCall -> MetadataMap -> StreamRecv ChaincodeMessage ->
--          StreamSend ChaincodeMessage -> WritesDone -> IO ()
biDiRequestFn chaincodeStub _call _mmap recv send _done = do
    e <- send regMessage :: IO (Either GRPCIOError ())
    case e of
        Left err -> error ("Error while streaming: " ++ show err)
        Right _ -> trace "okie dokey" pure ()
    chatWithPeer recv send chaincodeStub

chatWithPeer recv send chaincodeStub = do
    res <- recv
    case res of
        Left err -> error ("OH MY GOD: " ++ show err)
        Right (Just message) -> handler message send chaincodeStub
        Right Nothing -> print "I got no message... wtf"
    chatWithPeer recv send chaincodeStub

handler message send chaincodeStub = case message of
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeREGISTERED)} ->  print "YAY REGGED"
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeREADY)} ->  print "YAY READY"
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeINIT)} ->
        trace "YAY INIT" $ handleInit message send (initFn chaincodeStub)
    s ->  print ("Goodie:" ++ show s)


handleInit mes send initFn = let
    eErrInput = Suite.fromByteString (chaincodeMessagePayload mes) :: Either ParseError Pb.ChaincodeInput
    in
    case eErrInput of
        Left err -> error (show err)
        Right Pb.ChaincodeInput{chaincodeInputArgs= args} -> do
            e <- send (initCompletedMessage (chaincodeMessageTxid mes) (chaincodeMessageChannelId mes) (initFn stub)) :: IO (Either GRPCIOError ())
            case e of
                Left err -> error ("Error while streaming: " ++ show err)
                Right _ -> trace "okie dokey init" pure ()
