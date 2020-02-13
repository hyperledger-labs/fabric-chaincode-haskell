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
import Messages

import Debug.Trace

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "localhost"
                            , clientServerPort = 7052
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            , clientAuthority = Nothing
                            }

data ChaincodeStub = ChaincodeStub {
    initFn :: DefaultChaincodeStub -> Pb.Response,
    invokeFn :: DefaultChaincodeStub -> Pb.Response
}

-- Env to be accessible from anywhere in the application, this means stub functions can get access to env
-- without them needing to be passed from userland main function
-- data Env = Env {
--     send :: StreamSend ChaincodeMessage,
--     recv :: StreamRecv ChaincodeMessage
-- }
--
-- getSend :: Reader a (StreamSend ChaincodeMessage)
-- getSend = pure $ send

-- data HandlerType e a = HandlerType {
--     runHandlerType :: Env -> IO (Either e a)
-- }


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
        Right (Just message) -> handler message recv send chaincodeStub
        Right Nothing -> print "I got no message... wtf"
    chatWithPeer recv send chaincodeStub

handler message recv send chaincodeStub = case message of
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeREGISTERED)} ->  print "YAY REGGED"
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeREADY)} ->  print "YAY READY"
    ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeINIT)} ->
        trace "YAY INIT" $ handleInit message recv send (initFn chaincodeStub)
    -- ChaincodeMessage{chaincodeMessageType= Enumerated (Right ChaincodeMessage_TypeTRANSACTION)} ->
    --     trace "YAY TRANSACTION" $ handleInvoke message recv send (invokeFn chaincodeStub)
    s ->  print ("Goodie:" ++ show s)


handleInit mes recv send initFn = let
    eErrInput = Suite.fromByteString (chaincodeMessagePayload mes) :: Either ParseError Pb.ChaincodeInput
    stub  = DefaultChaincodeStub Nothing Nothing Nothing "HIHINI" Nothing Nothing Nothing Nothing Nothing Nothing Nothing recv send
    in
    case eErrInput of
        Left err -> error (show err)
        Right Pb.ChaincodeInput{chaincodeInputArgs= args} -> do
            e <- send (initCompletedMessage (chaincodeMessageTxid mes) (chaincodeMessageChannelId mes) (initFn stub)) :: IO (Either GRPCIOError ())
            case e of
                Left err -> error ("Error while streaming: " ++ show err)
                Right _ -> trace "okie dokey init" pure ()
