{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stub where

import qualified Common.Common                    as Pb

import           Control.Monad.Except             ( ExceptT(..), runExceptT, throwError )

import           Data.Bifunctor
import           Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Char                        ( chr )
import           Data.IORef                       ( modifyIORef, newIORef, readIORef, writeIORef )
import           Data.Text                        as TS
import           Data.Text.Encoding
import           Data.Text.Lazy                   as TL
import           Data.Vector                      as Vector ( (!), Vector, empty, foldr, length, toList )

import           Debug.Trace

import           Google.Protobuf.Timestamp        as Pb

import           Helper

import           Interfaces

import qualified Ledger.Queryresult.KvQueryResult as Pb

import           Messages

import           Network.GRPC.HighLevel

import qualified Peer.ChaincodeShim               as Pb
import           Peer.Proposal                    as Pb

import           Proto3.Suite
import           Proto3.Wire.Decode

import           Types

-- NOTE: When support for concurrency transaction is added, this function will no longer be required
-- as the stub function will block and listen for responses over a channel when the code is concurrent
listenForResponse :: StreamRecv Pb.ChaincodeMessage -> IO (Either Error ByteString)
listenForResponse recv = do
    res <- recv
    case res of
        Left err -> pure $ Left $ GRPCError err
        Right (Just Pb.ChaincodeMessage{ Pb.chaincodeMessageType = Enumerated (Right Pb.ChaincodeMessage_TypeRESPONSE)
                                       , Pb.chaincodeMessagePayload = payload
                                       }) -> pure $ Right payload
        Right (Just Pb.ChaincodeMessage{ Pb.chaincodeMessageType = Enumerated (Right Pb.ChaincodeMessage_TypeERROR)
                                       , Pb.chaincodeMessagePayload = payload
                                       }) -> pure $ Left $ Error "Peer failed to complete stub invocation request"
        Right (Just _) -> listenForResponse recv
        Right Nothing -> pure $ Left $ Error "Empty message received from peer"

instance ChaincodeStubInterface DefaultChaincodeStub where
      -- getArgs :: ccs -> Vector ByteString
    getArgs ccs = args ccs

    -- getStringArgs :: ccs -> [Text]
    getStringArgs ccs = let args = getArgs ccs in toList $ decodeUtf8 <$> args

    -- getFunctionAndParameters :: ccs -> Either Error (Text, [Text])
    getFunctionAndParameters ccs =
        let args = getStringArgs ccs
        in
            if not (Prelude.null args) then Right (Prelude.head args, Prelude.tail args) else Left InvalidArgs

    -- getArgsSlice :: ccs -> Either Error ByteString
    getArgsSlice ccs = Right $ Vector.foldr BS.append BS.empty $ getArgs ccs

    -- getTxId :: css -> String
    getTxId = txId

    -- getChannelId :: ccs -> String
    getChannelId = channelId

    -- getSignedProposal :: ccs -> Maybe Pb.SignedProposal
    getSignedProposal = signedProposal

    -- getCreator :: ccs -> Maybe ByteString
    getCreator = creator

    -- getTransient :: ccs -> Maybe MapTextBytes
    getTransient = transient

    -- getDecorations :: ccs -> MapTextBytes
    getDecorations = decorations

    -- getBinding :: ccs -> Maybe MapTextBytes
    getBinding = binding

    -- getTxTimestamp :: ccs -> Either Error Pb.Timestamp
    getTxTimestamp ccs = case (proposal ccs) of
        Just prop -> do
            header <- getHeader $ prop
            channelHeader <- getChannelHeader header
            case (Pb.channelHeaderTimestamp channelHeader) of
                Nothing -> Left $ Error "ChannelHeader doesn't have a timestamp"
                Just timestamp -> Right timestamp
        Nothing -> Left $ Error "Chaincode stub doesn't has a proposal to get the timestamp from"

    -- getState :: ccs -> Text ->  ExceptT Error IO ByteString
    getState ccs key =
        let payload = getStatePayload key
            message = buildChaincodeMessage GET_STATE payload (txId ccs) (channelId ccs)
        in
            ExceptT $ do
                e <- (sendStream ccs) message
                case e of
                    Left err -> pure $ Left $ Error $ "Error while streaming: " ++ show err
                    Right _ -> listenForResponse (recvStream ccs)

    -- putState :: ccs -> Text -> ByteString -> ExceptT Error IO ByteString
    putState ccs key value =
        let payload = putStatePayload key value
            message = buildChaincodeMessage PUT_STATE payload (txId ccs) (channelId ccs)
        in
            ExceptT $ do
                e <- (sendStream ccs) message
                case e of
                    Left err -> pure $ Left $ Error $ "Error while streaming: " ++ show err
                    Right _ -> listenForResponse (recvStream ccs)

    -- delState :: ccs -> Text -> IO (Maybe Error)
    delState ccs key =
        let payload = delStatePayload key
            message = buildChaincodeMessage DEL_STATE payload (txId ccs) (channelId ccs)
        in
            ExceptT $ do
                e <- (sendStream ccs) message
                case e of
                    Left err -> error ("Error while streaming: " ++ show err)
                    Right _ -> pure ()
                listenForResponse (recvStream ccs)

    -- TODO: Implement better error handling/checks etc
    -- getStateByRange :: ccs -> Text -> Text -> IO (Either Error StateQueryIterator)
    getStateByRange ccs startKey endKey =
        let payload = getStateByRangePayload startKey endKey Nothing
            message = buildChaincodeMessage GET_STATE_BY_RANGE payload (txId ccs) (channelId ccs)
        in
            ExceptT $ do
                e <- (sendStream ccs) message
                case e of
                    Left err -> error ("Error while streaming: " ++ show err)
                    Right _ -> pure ()
                runExceptT $ ExceptT (listenForResponse (recvStream ccs)) >>= (bsToSqi ccs)

    -- TODO: We need to implement this so we can test the fetchNextQueryResult functionality
      -- getStateByRangeWithPagination :: ccs -> Text -> Text -> Int -> Text -> IO (Either Error (StateQueryIterator, Pb.QueryResponseMetadata))
    getStateByRangeWithPagination ccs startKey endKey pageSize bookmark =
        let metadata = Pb.QueryMetadata { Pb.queryMetadataPageSize = fromIntegral pageSize
                                        , Pb.queryMetadataBookmark = TL.fromStrict bookmark
                                        }
            payload = getStateByRangePayload startKey endKey $ Just metadata
            message = buildChaincodeMessage GET_STATE_BY_RANGE payload (txId ccs) (channelId ccs)
        in
            ExceptT $ do
                e <- (sendStream ccs) message
                case e of
                    Left err -> error ("Error while streaming: " ++ show err)
                    Right _ -> pure ()
                runExceptT $ ExceptT (listenForResponse (recvStream ccs)) >>= (bsToSqiAndMeta ccs)

    -- TODO: This is the next TODO! Implement these 7 function because they are needed in marbles.hs
    -- getStateByPartialCompositeKey :: ccs -> Text -> [Text] -> Either Error StateQueryIterator
    getStateByPartialCompositeKey ccs objectType keys = throwError $ Error "not implemented"

    --getStateByPartialCompositeKeyWithPagination :: ccs -> Text -> [Text] -> Int32 -> Text -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    getStateByPartialCompositeKeyWithPagination ccs objectType keys pageSize bookmark =
        throwError $ Error "not implemented"

    --createCompositeKey :: ccs -> Text -> [Text] -> Either Error Text
    createCompositeKey ccs objectType keys =
        let keysString = Prelude.foldr (\key acc -> acc ++ TS.unpack key ++ nullCodepoint) "" keys
            nullCodepoint = [ chr 0 ]
        in
            -- TODO: Check that objectTypes and keys are all valid utf8 strings
            Right $ TS.pack $ "\x00" ++ TS.unpack objectType ++ nullCodepoint ++ keysString

    --splitCompositeKey :: ccs -> Text -> Either Error (Text, [Text])
    splitCompositeKey ccs key =
        -- key has the form \x00objectTypeU+0000keyU+0000key etc so we use `tail key` to ignore the \x00 char
        -- and then split on the unicode codepoint U+0000 to extract the objectType and keys
        let keys = TS.splitOn (TS.singleton $ chr 0) (TS.tail key) in Right (Prelude.head keys, Prelude.tail keys)

    --getQueryResult :: ccs -> Text -> Either Error StateQueryIterator
    getQueryResult ccs query = throwError $ Error "not implemented"

    --getQueryResultWithPagination :: ccs -> Text -> Int32 -> Text -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    getQueryResultWithPagination ccs key pageSize bookmark = throwError $ Error "not implemented"

    --getHistoryForKey :: ccs -> Text -> Either Error HistoryQueryIterator
    getHistoryForKey ccs key = throwError $ Error "not implemented"

instance StateQueryIteratorInterface StateQueryIterator where
    -- TODO: remove the IO from this function (possibly with the State monad)
      -- hasNext :: sqi -> IO Bool
    hasNext sqi = do
        queryResponse <- readIORef $ sqiResponse sqi
        -- (trace $ "Query response: " ++ show queryResponse)
        currentLoc <- readIORef $ sqiCurrentLoc sqi
        pure $ (currentLoc < Prelude.length (Pb.queryResponseResults queryResponse))
            || (Pb.queryResponseHasMore queryResponse)

    -- TODO : implement close function (need to do anything here in haskell?)
    -- close :: sqi -> IO (Maybe Error)
    close _ = pure Nothing

    -- next :: sqi -> IO (Either Error Pb.KV)
    next sqi = ExceptT $ do
        eeQueryResultBytes <- nextResult sqi
        case eeQueryResultBytes of
            Left _ -> pure $ Left $ Error "Error getting next queryResultBytes"
            -- TODO: use Suite.fromByteString
            Right queryResultBytes -> pure $
                first DecodeError
                      (parse (decodeMessage (FieldNumber 1)) (Pb.queryResultBytesResultBytes queryResultBytes)
                       :: Either ParseError Pb.KV)

-- ExceptT is a monad transformer that allows us to compose these by binding over IO Either
bsToSqi :: DefaultChaincodeStub -> ByteString -> ExceptT Error IO StateQueryIterator
bsToSqi ccs bs =
      -- TODO: use Suite.fromByteString
    let eeaQueryResponse = parse (decodeMessage (FieldNumber 1)) bs :: Either ParseError Pb.QueryResponse
    in
        case eeaQueryResponse of
            -- TODO: refactor out pattern matching, e.g. using >>= or <*>
            Left err -> ExceptT $ pure $ Left $ DecodeError err
            Right queryResponse -> ExceptT $ do
                -- queryResponse and currentLoc are IORefs as they need to be mutated
                -- as a part of the next() function 
                queryResponseIORef <- newIORef queryResponse
                currentLocIORef <- newIORef 0
                pure $ Right StateQueryIterator { sqiChaincodeStub = ccs
                                                , sqiChannelId     = getChannelId ccs
                                                , sqiTxId          = getTxId ccs
                                                , sqiResponse      = queryResponseIORef
                                                , sqiCurrentLoc    = currentLocIORef
                                                }

-- ExceptT is a monad transformer that allows us to compose these by binding over IO Either
bsToSqiAndMeta :: DefaultChaincodeStub -> ByteString -> ExceptT Error IO (StateQueryIterator, Pb.QueryResponseMetadata)
bsToSqiAndMeta ccs bs =
      -- TODO: use Suite.fromByteString
    let eeaQueryResponse = parse (decodeMessage (FieldNumber 1)) bs :: Either ParseError Pb.QueryResponse
    in
        case eeaQueryResponse of
            -- TODO: refactor out pattern matching, e.g. using >>= or <*>
            Left err -> ExceptT $ pure $ Left $ DecodeError err
            Right queryResponse ->
                    -- TODO: use Suite.fromByteString
                let eeMetadata = parse (decodeMessage (FieldNumber 1)) (Pb.queryResponseMetadata queryResponse)
                        :: Either ParseError Pb.QueryResponseMetadata
                in
                    case eeMetadata of
                        Left err -> ExceptT $ pure $ Left $ DecodeError err
                        Right metadata -> ExceptT $ do
                            -- queryResponse and currentLoc are IORefs as they need to be mutated
                            -- as a part of the next() function 
                            queryResponseIORef <- newIORef queryResponse
                            currentLocIORef <- newIORef 0
                            pure $ Right ( StateQueryIterator { sqiChaincodeStub = ccs
                                                              , sqiChannelId     = getChannelId ccs
                                                              , sqiTxId          = getTxId ccs
                                                              , sqiResponse      = queryResponseIORef
                                                              , sqiCurrentLoc    = currentLocIORef
                                                              }
                                         , metadata
                                         )

nextResult :: StateQueryIterator -> IO (Either Error Pb.QueryResultBytes)
nextResult sqi = do
    currentLoc <- readIORef $ sqiCurrentLoc sqi
    queryResponse <- readIORef $ sqiResponse sqi
    -- Checking if there are more local results
    if (currentLoc < Prelude.length (Pb.queryResponseResults $ queryResponse))
        then let queryResult = pure $ Right $ (Pb.queryResponseResults $ queryResponse) ! currentLoc
             in
                 do
                     modifyIORef (sqiCurrentLoc sqi) (+ 1)
                     if ((currentLoc + 1) == Prelude.length (Pb.queryResponseResults $ queryResponse))
                         then do
                             fetchNextQueryResult sqi
                             queryResult
                         else queryResult
        else pure $ Left $ Error "Invalid iterator state"

-- This function is only called when the local result list has been 
-- iterated through and there are more results to get from the peer
-- It makes a call to get the next QueryResponse back from the peer 
-- and mutates the sqi with the new QueryResponse and sets currentLoc back to 0
fetchNextQueryResult :: StateQueryIterator -> IO (Either Error StateQueryIterator)
fetchNextQueryResult sqi = do
    queryResponse <- readIORef $ sqiResponse sqi
    let payload = queryNextStatePayload $ TL.toStrict $ Pb.queryResponseId queryResponse
        message = buildChaincodeMessage QUERY_STATE_NEXT payload (sqiTxId sqi) (sqiChannelId sqi)
        bsToQueryResponse :: ByteString -> ExceptT Error IO StateQueryIterator
        bsToQueryResponse bs =
            let eeaQueryResponse =
                    -- TODO: Suite.fromByteString
                    parse (decodeMessage (FieldNumber 1)) bs :: Either ParseError Pb.QueryResponse
            in
                case eeaQueryResponse of
                    -- TODO: refactor out pattern matching, e.g. using >>= or <*>
                    Left err -> ExceptT $ pure $ Left $ DecodeError err
                    Right queryResponse -> ExceptT $ do
                        -- Need to put the new queryResponse in the sqi queryResponse
                        writeIORef (sqiCurrentLoc sqi) 0
                        writeIORef (sqiResponse sqi) queryResponse
                        pure $ Right sqi
        in
            do
                e <- (sendStream $ sqiChaincodeStub sqi) message
                case e of
                    Left err -> error ("Error while streaming: " ++ show err)
                    Right _ -> pure ()
                runExceptT $ ExceptT (listenForResponse (recvStream $ sqiChaincodeStub sqi)) >>= bsToQueryResponse
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
-- -- setEvent :: ccs -> String -> ByteArray -> Maybe Error
-- setEvent ccs = Right notImplemented
