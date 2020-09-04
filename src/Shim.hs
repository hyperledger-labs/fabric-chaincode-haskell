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
  , StateQueryIterator(..)
  , StateQueryIteratorInterface(..)
  )
where
import           Data.Bifunctor                ( first )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Char8         as BC
import           Data.Map                       ( mapKeys )
import           Data.Text.Encoding            as TSE
import           Data.Text
import           Data.Text.Lazy                 ( toStrict )

-- import           Crypto.Hash.SHA256

import           Network.GRPC.HighLevel.Generated
import           Network.GRPC.HighLevel
import qualified Network.GRPC.LowLevel.Client  as Client
import           Proto3.Suite                  as Suite

import           Common.Common                 as Pb
import           Peer.ChaincodeShim            as Pb
import           Peer.Chaincode                as Pb
import           Peer.Proposal                 as Pb
import           Peer.ProposalResponse         as Pb

import           Stub
import           Interfaces                     ( ChaincodeStubInterface(..)
                                                , StateQueryIteratorInterface(..)
                                                )
import           Messages
import           Types                          ( DefaultChaincodeStub(..)
                                                , Error(..)
                                                , ChaincodeStub(..)
                                                , MapTextBytes
                                                , StateQueryIterator(..)
                                                )
import          Helper

import           Debug.Trace

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "localhost"
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

  putStrLn "Could not connect to peer"

-- biDiRequestFn :: ClientCall -> MetadataMap -> StreamRecv ChaincodeMessage ->
--          StreamSend ChaincodeMessage -> WritesDone -> IO ()
biDiRequestFn chaincodeStub _call _mmap recv send _done = do
  e <- send regMessage :: IO (Either GRPCIOError ())
  case e of
    Left  err -> error ("Error registering with peer: " ++ show err)
    Right _   -> trace "Registering with peer" pure ()
  chatWithPeer recv send chaincodeStub

-- main loop listening for messages from the peer
chatWithPeer :: IO (Either GRPCIOError (Maybe ChaincodeMessage)) -> StreamSend ChaincodeMessage -> ChaincodeStub -> IO b
chatWithPeer recv send chaincodeStub = do
  res <- recv
  case res of
    Left err -> error ("Error during communication with peer: " ++ show err)
    Right (Just message) -> handler message recv send chaincodeStub
    Right Nothing -> putStrLn "Empty message received from peer"
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
    -> putStrLn "REGISTERED message received from the peer"
  ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeREADY) }
    -> putStrLn "READY message received from the peer"
  ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeINIT) }
    -> trace "INIT message received from the peer"
      $ handleInit message recv send (initFn chaincodeStub)
  ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeTRANSACTION) }
    -> trace "TRANSACTION message received from the peer"
      $ handleInvoke message recv send (invokeFn chaincodeStub)
  s -> putStrLn ("Unknown message received from peer:" ++ show s)

handleInit
  :: ChaincodeMessage
  -> StreamRecv ChaincodeMessage
  -> StreamSend ChaincodeMessage
  -> (DefaultChaincodeStub -> IO Pb.Response)
  -> IO ()
handleInit mes recv send initFn =
  let eStub = newChaincodeStub mes recv send
  in  case eStub of
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
            Right _   -> pure ()


handleInvoke
  :: ChaincodeMessage
  -> StreamRecv ChaincodeMessage
  -> StreamSend ChaincodeMessage
  -> (DefaultChaincodeStub -> IO Pb.Response)
  -> IO ()
handleInvoke mes recv send invokeFn =
  let eStub = newChaincodeStub mes recv send
  in  case eStub of
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
            Right _   -> pure ()

newChaincodeStub
  :: ChaincodeMessage
  -> StreamRecv ChaincodeMessage
  -> StreamSend ChaincodeMessage
  -> Either Error DefaultChaincodeStub
newChaincodeStub mes recv send = do
  input <- getChaincodeInput mes
  let maybeSignedProposal = chaincodeMessageProposal mes
      in  case maybeSignedProposal of
          --  If the SignedProposal is empty, populate the stub with just the
          -- args, txId, channelId, decorations, send and recv
            Nothing -> Right $ DefaultChaincodeStub{
                          args = chaincodeInputArgs input
                          , txId = toStrict $ chaincodeMessageTxid mes
                          , channelId = toStrict $ chaincodeMessageChannelId mes
                          , creator = Nothing
                          , signedProposal = Nothing 
                          , proposal = Nothing
                          , transient = Nothing
                          , binding = Nothing
                          , decorations = chaincodeInputDecorations input
                          , recvStream = recv
                          , sendStream = send
                    }
            --  If SignedProposal is not empty, get the proposal from it
            -- and the creator, transient and binding from the proposal
            Just signedProposal -> do
              proposal <- getProposal signedProposal
              header <- getHeader proposal
              chaincodeProposalPayload <- getChaincodeProposalPayload proposal
              channelHeader <- getChannelHeader header
              signatureHeader <- getSignatureHeader header
              Right $ DefaultChaincodeStub{
                          args = chaincodeInputArgs input
                          , txId = toStrict $ chaincodeMessageTxid mes
                          , channelId = toStrict $ chaincodeMessageChannelId mes
                          , creator = Just $ signatureHeaderCreator signatureHeader
                          , signedProposal = Just signedProposal
                          , proposal = Just proposal
                          , transient = Just $ chaincodeProposalPayloadTransientMap chaincodeProposalPayload
                          , binding = createBinding proposal
                          , decorations = chaincodeInputDecorations input
                          , recvStream = recv
                          , sendStream = send
                    }
                           
