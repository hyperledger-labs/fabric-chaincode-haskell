{-# LANGUAGE OverloadedStrings #-}

module Stub where


import           Data.ByteString               as BS
import           Data.Text
import           Data.Text.Encoding
import           Data.Vector                   as Vector
                                                ( Vector
                                                , length
                                                , toList
                                                , foldr
                                                , empty
                                                )

import           Peer.ChaincodeShim

import           Network.GRPC.HighLevel
import           Google.Protobuf.Timestamp     as Pb
import           Peer.Proposal                 as Pb
import           Proto3.Suite

import           Interfaces
import           Messages
import           Types


-- NOTE: When support for concurrency transaction is added, this function will no longer be required
-- as the stub function will block and listen for responses over a channel when the code is concurrent
listenForResponse :: StreamRecv ChaincodeMessage -> IO (Either Error ByteString)
listenForResponse recv = do
  res <- recv
  case res of
    Left err -> pure $ Left $ GRPCError err
    Right (Just ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeRESPONSE), chaincodeMessagePayload = payload })
      -> pure $ Right payload
    Right (Just ChaincodeMessage { chaincodeMessageType = Enumerated (Right ChaincodeMessage_TypeERROR), chaincodeMessagePayload = payload })
      -> pure $ Left $ Error "Peer failed to do what you wanted"
    Right (Just _) -> listenForResponse recv
    Right Nothing  -> pure $ Left $ Error "I got no message... wtf"

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
  getState :: ccs -> Text -> IO (Either Error ByteString)
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

  -- -- putState :: ccs -> Text -> ByteString -> Maybe Error
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
    -- getStateByRange :: ccs -> Text -> Text -> Either Error StateQueryIterator
    getStateByRange ccs startKey endKey = 
      let payload = getStateByRangePayload startKey endKey
          message = buildChaincodeMessage GET_STATE_BY_RANGE payload (txId ccs) (channelId ccs) 
      in do 
        e <- (sendStream ccs) message
        case e of
          Left err -> error ("Error while streaming: " ++ show err)
          Right _ -> pure ()
        listenForResponse (recvStream ccs) 
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

instance StateQueryIteratorInterface DefaultStateQueryIterator where
    -- hasNext :: sqi -> Bool
    hasNext sqi = True
    -- close :: sqi -> Maybe Error
    close _ = Nothing
    -- next :: sqi -> Either Error Pb.KV
    next _ = Left _