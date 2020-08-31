{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stub where


import           Data.Bifunctor
import           Data.ByteString               as BS
import           Data.Text
import           Data.Text.Lazy                as TL
import           Data.Text.Encoding
import           Data.IORef                     ( readIORef
                                                , newIORef
                                                , modifyIORef
                                                , writeIORef
                                                )
import           Data.Vector                   as Vector
                                                ( Vector
                                                , length
                                                , toList
                                                , foldr
                                                , empty
                                                , (!)
                                                )
import qualified Data.ByteString.Lazy          as LBS
import           Data.IORef                    (readIORef, newIORef, modifyIORef)
import           Control.Monad.Except          (ExceptT(..), runExceptT)

import qualified Peer.ChaincodeShim            as Pb
import qualified Ledger.Queryresult.KvQueryResult as Pb

import           Network.GRPC.HighLevel
import           Google.Protobuf.Timestamp     as Pb
import           Peer.Proposal                 as Pb
import           Proto3.Suite
import           Proto3.Wire.Decode

import           Interfaces
import           Messages
import           Types

-- NOTE: When support for concurrency transaction is added, this function will no longer be required
-- as the stub function will block and listen for responses over a channel when the code is concurrent
listenForResponse :: StreamRecv Pb.ChaincodeMessage -> IO (Either Error ByteString)
listenForResponse recv = do
  res <- recv
  case res of
    Left err -> pure $ Left $ GRPCError err
    Right (Just Pb.ChaincodeMessage { Pb.chaincodeMessageType = Enumerated (Right Pb.ChaincodeMessage_TypeRESPONSE), Pb.chaincodeMessagePayload = payload })
      -> pure $ Right payload
    Right (Just Pb.ChaincodeMessage { Pb.chaincodeMessageType = Enumerated (Right Pb.ChaincodeMessage_TypeERROR), Pb.chaincodeMessagePayload = payload })
      -> pure $ Left $ Error "Peer failed to complete stub invocation request"
    Right (Just _) -> listenForResponse recv
    Right Nothing  -> pure $ Left $ Error "Empty message received from peer"

instance ChaincodeStubInterface DefaultChaincodeStub where
    -- getArgs :: ccs -> Vector ByteString
  getArgs ccs = args ccs

  -- getStringArgs :: ccs -> [Text]
  getStringArgs ccs = let args = getArgs ccs in toList $ decodeUtf8 <$> args

  -- getFunctionAndParameters :: ccs -> Either Error (Text, [Text])
  getFunctionAndParameters ccs =
    let args = getStringArgs ccs
    in  if not (Prelude.null args)
          then Right (Prelude.head args, Prelude.tail args)
          else Left InvalidArgs

  -- getArgsSlice :: ccs -> Either Error ByteString
  getArgsSlice ccs = Right $ Vector.foldr BS.append BS.empty $ getArgs ccs

  -- getTxId :: css -> String
  getTxId css = txId css

  -- getChannelId :: ccs -> String
  getChannelId ccs = channelId ccs

  -- invokeChaincode :: ccs -> String -> [ByteString] -> String -> Pb.Response
  -- invokeChaincode ccs cc params = Pb.Response{ responseStatus = 500, responseMessage = message(notImplemented), responsePayload = Nothing }
  --
  -- getState :: ccs -> Text -> IO (Either Error ByteString)
  getState ccs key =
    let payload = getStatePayload key
        message =
            buildChaincodeMessage GET_STATE payload (txId ccs) (channelId ccs)
    in  do
          e <- (sendStream ccs) message
          case e of
            Left  err -> error ("Error while streaming: " ++ show err)
            Right _   -> pure ()
          listenForResponse (recvStream ccs)

  -- putState :: ccs -> Text -> ByteString -> Maybe Error
  putState ccs key value =
    let payload = putStatePayload key value
        message =
            buildChaincodeMessage PUT_STATE payload (txId ccs) (channelId ccs)
    in  do
          e <- (sendStream ccs) message
          case e of
            Left  err -> error ("Error while streaming: " ++ show err)
            Right _   -> pure ()
          listenForResponse (recvStream ccs)

  -- delState :: ccs -> Text -> IO (Maybe Error)
  delState ccs key =
      let payload = delStatePayload key
          message = buildChaincodeMessage DEL_STATE payload (txId ccs) (channelId ccs)
      in do
        e <- (sendStream ccs) message
        case e of
          Left err -> error ("Error while streaming: " ++ show err)
          Right _ -> pure ()
        listenForResponse (recvStream ccs)

    --
    -- -- setStateValidationParameter :: ccs -> String -> [ByteString] -> Maybe Error
    -- setStateValidationParameter ccs key parameters = Right notImplemented
    --
    -- -- getStateValiationParameter :: ccs -> String -> Either Error [ByteString]
    -- getStateValiationParameter ccs key = Left notImplemented
    --

  -- TODO: Implement better error handling/checks etc
  -- getStateByRange :: ccs -> Text -> Text -> IO (Either Error StateQueryIterator)
  getStateByRange ccs startKey endKey =
    let payload = getStateByRangePayload startKey endKey
        message = buildChaincodeMessage GET_STATE_BY_RANGE payload (txId ccs) (channelId ccs) 
        -- We have listenForResponse a :: IO (Either Error ByteString)
        -- and the function bsToSqi :: ByteString -> IO (Either Error StateQueryIterator)
        -- And want IO (Either Error StateQueryIterator)
        -- ExceptT is a monad transformer that allows us to compose these by binding over IO Either
        bsToSqi :: ByteString -> ExceptT Error IO StateQueryIterator
        bsToSqi bs = let eeaQueryResponse = parse (decodeMessage (FieldNumber 1)) bs :: Either ParseError Pb.QueryResponse in
          case eeaQueryResponse of
                  -- TODO: refactor out pattern matching, e.g. using >>= or <*>
                  Left  err             -> ExceptT $ pure $ Left $ DecodeError err
                  Right queryResponse -> ExceptT $ do
                    -- queryResponse and currentLoc are IORefs as they need to be mutated
                    -- as a part of the next() function 
                    queryResponseIORef <- newIORef queryResponse
                    currentLocIORef    <- newIORef 0
                    pure $ Right StateQueryIterator
                      { sqiChaincodeStub = ccs 
                      , sqiChannelId     = getChannelId ccs
                      , sqiTxId          = getTxId ccs
                      , sqiResponse      = queryResponseIORef
                      , sqiCurrentLoc    = currentLocIORef
                      }
    in  do
          e <- (sendStream ccs) message
          case e of
            Left  err -> error ("Error while streaming: " ++ show err)
            Right _   -> pure ()
          runExceptT $ ExceptT (listenForResponse (recvStream ccs)) >>= bsToSqi

  -- TODO : implement all these interface functions
instance StateQueryIteratorInterface StateQueryIterator where
-- TODO: remove the IO from this function (possibly with the State monad)
    -- hasNext :: sqi -> IO Bool
  hasNext sqi = do
    queryResponse <- readIORef $ sqiResponse sqi
    currentLoc <- readIORef $ sqiCurrentLoc sqi
    pure $ currentLoc < Prelude.length (Pb.queryResponseResults queryResponse) || (Pb.queryResponseHasMore queryResponse)
  -- close :: sqi -> IO (Maybe Error)
  close _ = pure Nothing
  -- next :: sqi -> IO (Either Error Pb.KV)
  next sqi = do
    eeQueryResultBytes <- nextResult sqi 
    case eeQueryResultBytes of
      Left _ -> pure $ Left $ Error "Error getting next queryResultBytes"
      Right queryResultBytes -> pure $ first DecodeError (parse (decodeMessage (FieldNumber 1)) (Pb.queryResultBytesResultBytes queryResultBytes) :: Either ParseError Pb.KV)


nextResult :: StateQueryIterator -> IO (Either Error Pb.QueryResultBytes)
nextResult sqi = do
  currentLoc <- readIORef $ sqiCurrentLoc sqi
  queryResponse <- readIORef $ sqiResponse sqi
  -- Checking if there are more local results
  if (currentLoc < Prelude.length (Pb.queryResponseResults $ queryResponse)) then
    let queryResult = pure $ Right $ (Pb.queryResponseResults $ queryResponse) ! currentLoc in
      do
        modifyIORef (sqiCurrentLoc sqi) (+ 1)
        if ((currentLoc + 1) == Prelude.length (Pb.queryResponseResults $ queryResponse)) then
          do
            fetchNextQueryResult sqi
            queryResult
        else
          queryResult
  else pure $ Left $ Error "Invalid iterator state"


-- This function is only called when the local result list has been 
-- iterated through and there are more results to get from the peer
-- It makes a call to get the next QueryResponse back from the peer 
-- and mutates the sqi with the new QueryResponse and sets currentLoc back to 0
fetchNextQueryResult :: StateQueryIterator -> IO (Either Error StateQueryIterator)
fetchNextQueryResult sqi = do
  queryResponse <- readIORef $ sqiResponse sqi
  let 
    payload = queryNextStatePayload $ TL.toStrict $ Pb.queryResponseId queryResponse 
    message = buildChaincodeMessage QUERY_STATE_NEXT payload (sqiTxId sqi) (sqiChannelId sqi)
    bsToQueryResponse :: ByteString -> ExceptT Error IO StateQueryIterator
    bsToQueryResponse bs =
      let eeaQueryResponse =
              parse (decodeMessage (FieldNumber 1)) bs :: Either
                  ParseError
                  Pb.QueryResponse
      in  case eeaQueryResponse of
            -- TODO: refactor out pattern matching, e.g. using >>= or <*>
            Left  err             -> ExceptT $ pure $ Left $ DecodeError err
            Right queryResponse -> ExceptT $ do
            -- Need to put the new queryResponse in the sqi queryResponse
              writeIORef (sqiCurrentLoc sqi) 0
              writeIORef (sqiResponse sqi) queryResponse
              pure $ Right sqi
    in do 
      e <- (sendStream $ sqiChaincodeStub sqi) message
      case e of
          Left  err -> error ("Error while streaming: " ++ show err)
          Right _   -> pure ()
      runExceptT $ ExceptT (listenForResponse (recvStream $ sqiChaincodeStub sqi)) >>= bsToQueryResponse
    
    --
    -- -- getStateByRangeWithPagination :: ccs -> String -> String -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- getStateByRangeWithPagination ccs startKey endKey pageSize bookmark = Left notImplemented
    --
    -- -- getStateByPartialCompositeKey :: ccs -> String -> [String] -> Either Error StateQueryIterator
    -- getStateByPartialCompositeKey ccs objectType keys  = Left notImplemented
    --
    -- --getStateByPartialCompositeKeyWithPagination :: ccs -> String -> [String] -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- getStateByPartialCompositeKeyWithPagination ccs objectType keys pageSize bookmark = Left notImplemented
    --
    -- --createCompositeKey :: ccs -> String -> [String] -> Either Error String
    -- createCompositeKey ccs objectType keys = Left notImplemented
    --
    -- --splitCompositeKey :: ccs -> String -> Either Error (String, [String])
    -- splitCompositeKey ccs key = Left notImplemented
    --
    -- --getQueryResult :: ccs -> String -> Either Error StateQueryIterator
    -- getQueryResult ccs query = Left notImplemented
    --
    -- --getQueryResultWithPagination :: ccs -> String -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- getQueryResultWithPagination ccs key pageSize bookmark = Left notImplemented
    --
    -- --getHistoryForKey :: ccs -> String -> Either Error HistoryQueryIterator
    -- getHistoryForKey ccs key = Left notImplemented
    --
    -- --getPrivateData :: ccs -> String -> String -> Either Error ByteString
    -- getPrivateData ccs collection key = Left notImplemented
    --
    -- --getPrivateDataHash :: ccs -> String -> String -> Either Error ByteString
    -- getPrivateDataHash ccs collection key = Left notImplemented
    --
    -- --putPrivateData :: ccs -> String -> String -> ByteString -> Maybe Error
    -- putPrivateData ccs collection string value = Right notImplemented
    --
    -- --delPrivateData :: ccs -> String -> String -> Maybe Error
    -- delPrivateData ccs collection key = Right notImplemented
    --
    -- --setPrivateDataValidationParameter :: ccs -> String -> String -> ByteArray -> Maybe Error
    -- setPrivateDataValidationParameter ccs collection key params = Right notImplemented
    --
    -- --getPrivateDataValidationParameter :: ccs -> String -> String -> Either Error ByteString
    -- getPrivateDataValidationParameter ccs collection key = Left notImplemented
    --
    -- --getPrivateDataByRange :: ccs -> String -> String -> String -> Either Error StateQueryIterator
    -- getPrivateDataByRange ccs collection startKey endKey = Left notImplemented
    --
    -- --getPrivateDataByPartialCompositeKey :: ccs -> String -> String -> [String] -> Either Error StateQueryIterator
    -- getPrivateDataByPartialCompositeKey ccs collection objectType keys = Left notImplemented
    --
    -- -- getPrivateDataQueryResult :: ccs -> String -> String -> Either Error StateQueryIterator
    -- getPrivateDataQueryResult ccs collection query  = Left notImplemented
    --
    -- -- getCreator :: ccs -> Either Error ByteArray
    -- getCreator ccs = Right creator
    --
    -- -- getTransient :: ccs -> Either Error MapStringBytes
    -- getTransient ccs = Right transient
    --
    -- -- getBinding :: ccs -> Either Error MapStringBytes
    -- getBinding ccs = Right binding
    --
    -- -- getDecorations :: ccs -> MapStringBytes
    -- getDecorations ccs = Right decorations
    --
    -- -- getSignedProposal :: ccs -> Either Error Pb.SignedProposal
    -- getSignedProposal ccs = Right signedProposal
    --
    -- -- getTxTimestamp :: ccs -> Either Error Pb.Timestamp
    -- getTxTimestamp ccs = Right txTimestamp
    --
    -- -- setEvent :: ccs -> String -> ByteArray -> Maybe Error
    -- setEvent ccs = Right notImplemented
