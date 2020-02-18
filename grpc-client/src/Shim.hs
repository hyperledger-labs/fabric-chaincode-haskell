{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedLists   #-}

module Shim
  ( start
  , DefaultChaincodeStub(..)
  , ChaincodeStub(..)
  , Error(..)
  , errorPayload
  , successPayload
  , ChaincodeStubInterface(..)
  )
where

import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString.Char8         as BC
import           Data.Map                       ( mapKeys )
import           Data.Text.Encoding            as TSE
import           Data.Text
import           Data.Text.Lazy                 ( toStrict )

import           Network.GRPC.HighLevel.Generated
import           Network.GRPC.HighLevel
import qualified Network.GRPC.LowLevel.Client  as Client
import           Proto3.Suite                  as Suite
import           Proto3.Wire.Encode            as Wire
import           Proto3.Wire.Decode            as Wire
import           Proto3.Wire

import           Peer.ChaincodeShim            as Pb
import           Peer.Chaincode                as Pb
import           Peer.ProposalResponse         as Pb

import           Stub
import           Interfaces                     ( ChaincodeStubInterface(..) )
import           Messages
import           Types                          ( DefaultChaincodeStub(..)
                                                , Error(..)
                                                , ChaincodeStub(..)
                                                )

import           Debug.Trace

clientConfig :: ClientConfig
clientConfig = ClientConfig
  { clientServerHost = "localhost"
  , clientServerPort = 7052
  , clientArgs       = []
  , clientSSLConfig  = Nothing
  , clientAuthority  = Nothing
  }

-- TODO: start :: ChaincodeStub a => (a -> Pb.Response) -> IO ()
start :: ChaincodeStub -> IO ()
start chaincodeStub = withGRPCClient clientConfig $ grpcRunner chaincodeStub

-- initialise the GRPC communication with the peer
grpcRunner :: ChaincodeStub -> Client.Client -> IO ()
grpcRunner chaincodeStub client = do
  -- contains chaincodeSupportRegister function
  Pb.ChaincodeSupport {..} <- chaincodeSupportClient client


  -- NOTE: This 2000 seconds is a hack till this gets resolved https://github.com/awakesecurity/gRPC-haskell/issues/100
  _ <- chaincodeSupportRegister $ ClientBiDiRequest 2000 [] $ biDiRequestFn
    chaincodeStub

  print "testing"

-- biDiRequestFn :: ClientCall -> MetadataMap -> StreamRecv ChaincodeMessage ->
--          StreamSend ChaincodeMessage -> WritesDone -> IO ()
biDiRequestFn chaincodeStub _call _mmap recv send _done = do
  e <- send regMessage :: IO (Either GRPCIOError ())
  case e of
    Left  err -> error ("Error while streaming: " ++ show err)
    Right _   -> trace "okie dokey" pure ()
  chatWithPeer recv send chaincodeStub

-- main loop listening for messages from the peer
chatWithPeer recv send chaincodeStub = do
  res <- recv
  case res of
    Left  err            -> error ("OH MY GOD: " ++ show err)
    Right (Just message) -> handler message recv send chaincodeStub
    Right Nothing        -> print "I got no message... wtf"
  chatWithPeer recv send chaincodeStub

-- function to process the different chainccode message types
handler
  :: ChaincodeMessage
  -> StreamRecv ChaincodeMessage
  -> StreamSend ChaincodeMessage
  -> ChaincodeStub
  -> IO ()
handler message recv send chaincodeStub = case message of
  ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeREGISTERED) }
    -> print "YAY REGGED"
  ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeREADY) }
    -> print "YAY READY"
  ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeINIT) }
    -> trace "YAY INIT" $ handleInit message recv send (initFn chaincodeStub)
  ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeTRANSACTION) }
    -> trace "YAY TRANSACTION"
      $ handleInvoke message recv send (invokeFn chaincodeStub)
  s -> print ("Goodie:" ++ show s)

handleInit
  :: ChaincodeMessage
  -> StreamRecv ChaincodeMessage
  -> StreamSend ChaincodeMessage
  -> (DefaultChaincodeStub -> IO Pb.Response)
  -> IO ()
handleInit mes recv send initFn
  = let eStub = newChaincodeStub mes recv send
    in
      case eStub of
        Left  err  -> error ("Error while creating stub: " ++ show err)
        Right stub -> do
          response <- initFn stub
          e        <-
            send
              (buildChaincodeMessage COMPLETED
                                     response
                                     (getTxId stub)
                                     (getChannelId stub)
              ) :: IO (Either GRPCIOError ())
          case e of
            Left  err -> error ("Error while streaming: " ++ show err)
            Right _   -> trace "okie dokey init" pure ()


handleInvoke
  :: ChaincodeMessage
  -> StreamRecv ChaincodeMessage
  -> StreamSend ChaincodeMessage
  -> (DefaultChaincodeStub -> IO Pb.Response)
  -> IO ()
handleInvoke mes recv send invokeFn
  = let eStub = newChaincodeStub mes recv send
    in
      case eStub of
        Left  err  -> error ("Error while creating stub: " ++ show err)
        Right stub -> do
          response <- invokeFn stub
          e        <-
            send
              (buildChaincodeMessage COMPLETED
                                     response
                                     (getTxId stub)
                                     (getChannelId stub)
              ) :: IO (Either GRPCIOError ())
          case e of
            Left  err -> error ("Error while streaming: " ++ show err)
            Right _   -> trace "okie dokey invoke transaction" pure ()

-- TODO: extract proposal from signedProposal
-- then extract creator, transient and binding from proposal
newChaincodeStub
  :: ChaincodeMessage
  -> StreamRecv ChaincodeMessage
  -> StreamSend ChaincodeMessage
  -> Either Error DefaultChaincodeStub
newChaincodeStub mes recv send
  = let eErrInput =
          Suite.fromByteString (chaincodeMessagePayload mes) :: Either
              ParseError
              Pb.ChaincodeInput
    in
      case eErrInput of
        Left err -> Left $ error (show err)
        Right Pb.ChaincodeInput { chaincodeInputArgs = args, chaincodeInputDecorations = decorations }
          -> let signedProposal = chaincodeMessageProposal mes
             in  Right $ DefaultChaincodeStub
                   args
                   (toStrict $ chaincodeMessageTxid mes)
                   (toStrict $ chaincodeMessageChannelId mes)
                   Nothing
                   signedProposal
                   Nothing
                   Nothing
                   (mapKeys toStrict decorations)
                   recv
                   send
