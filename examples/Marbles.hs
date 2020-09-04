{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Marbles where

import           Control.Monad.Except             ( ExceptT(..), runExceptT, throwError )

import           Data.Aeson                       ( FromJSON
                                                  , ToJSON
                                                  , decode
                                                  , defaultOptions
                                                  , encode
                                                  , genericToEncoding
                                                  , toEncoding
                                                  )
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.UTF8             as BSU
import           Data.Functor.Classes
import           Data.Text                        ( Text, append, pack, unpack )
import qualified Data.Text.Encoding               as TSE
import qualified Data.Text.Lazy                   as TL

import           Debug.Trace

import           GHC.Generics

import           Ledger.Queryresult.KvQueryResult as Pb

import           Peer.ChaincodeShim               as Pb
import           Peer.ProposalResponse            as Pb

import           Shim                             ( ChaincodeStub(..)
                                                  , ChaincodeStubInterface(..)
                                                  , DefaultChaincodeStub
                                                  , Error(..)
                                                  , StateQueryIterator(..)
                                                  , StateQueryIteratorInterface(..)
                                                  , errorPayload
                                                  , start
                                                  , successPayload
                                                  )

main :: IO ()
main = Shim.start chaincodeStub

chaincodeStub :: ChaincodeStub
chaincodeStub = ChaincodeStub { initFn   = initFunc
                              , invokeFn = invokeFunc
                              }

data Marble = Marble { objectType :: Text
                     , name       :: Text
                     , color      :: Text
                     , size       :: Text
                     , owner      :: Text
                     }
    deriving ( Generic, Show )

instance ToJSON Marble where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Marble

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s = let e = getFunctionAndParameters s
             in
                 case e of
                     Left _ -> pure $ errorPayload ""
                     Right ("initMarble", parameters) -> initMarble s parameters
                     Right (fn, _) -> pure $ errorPayload (pack ("Invoke did not find function: " ++ unpack fn))

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
    let e = getFunctionAndParameters s
    in
        case e of
            Left _ -> pure $ errorPayload ""
            Right ("initMarble", parameters) -> initMarble s parameters
            Right ("transferMarble", parameters) -> transferMarble s parameters
            -- Right ("transferMarbleBasedOnColor", parameters) ->
            --   transferMarbleBasedOnColor s parameters
            Right ("deleteMarble", parameters) -> deleteMarble s parameters
            Right ("readMarble", parameters) -> readMarble s parameters
            -- Right ("queryMarblesByOwner", parameters) ->
            --   queryMarblesByOwner s parameters
            -- Right ("queryMarbles", parameters) -> queryMarbles s parameters
            -- Right ("getHistoryForMarble", parameters) ->
            --   getHistoryForMarble s parameters
            Right ("getMarblesByRange", parameters) -> getMarblesByRange s parameters
            Right ("getMarblesByRangeWithPagination", parameters) -> getMarblesByRangeWithPagination s parameters
            -- Right ("queryMarblesWithPagination", parameters) ->
            --   queryMarblesWithPagination s parameters
            Right (fn, _) -> pure $ errorPayload (pack ("Invoke did not find function: " ++ unpack fn))

-- TODO: implement CreateCompositeKey to index the marble by color
initMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
initMarble s params =
    if Prelude.length params == 4
    then eitherToPbResponse
        <$> (runExceptT $ do
                 response <- getState s (head params)
                 -- Check if marble already exists
                 if BS.length response /= 0
                     then throwError $ Error $ "This marble already exists: " ++ (unpack $ head params)
                     else 
                         -- marshal marble to JSON
                         let marbleJSON = LBS.toStrict $ encode (parseMarble params)
                         in
                             putState s (head params) marbleJSON)
    else pure $ errorPayload "Incorrect arguments. Need a marble name, color, size and owner"

transferMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
transferMarble s params =
    if Prelude.length params == 2
    then eitherToPbResponse
        <$> (runExceptT $ do
                 response <- getState s (head params)
                 if BS.length response == 0
                     then throwError $ Error $ "Marble not found"
                     else 
                         -- Unmarshal the marble from JSON
                         let maybeMarble = decode (LBS.fromStrict response) :: Maybe Marble
                             marbleOwner = params !! 1
                         in
                             case maybeMarble of
                                 Nothing -> throwError $ Error "Error decoding marble"
                                 Just oldMarble ->
                                     -- Create a new marble instance with the new owner
                                     let newMarble = marbleWithNewOwner marbleOwner oldMarble
                                         -- Marshal new marble to JSON
                                         marbleJSON = LBS.toStrict $ encode newMarble
                                     in
                                         putState s (head params) marbleJSON)
    else pure $ errorPayload "Incorrect arguments. Need a marble name and new owner"

-- -- TODO: Once indexing by color has been implemented, need to
-- -- get marble and also delete marble composite key
deleteMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
deleteMarble s params = if Prelude.length params == 1
                        then eitherToPbResponse <$> (runExceptT $ do
                                                         _ <- delState s (head params)
                                                         pure $ successPayload Nothing)
                        else pure $ errorPayload "Incorrect arguments. Need a marble name"

readMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
readMarble s params = if Prelude.length params == 1
                      then eitherToPbResponse <$> (runExceptT $ do
                                                       response <- getState s (head params)
                                                       trace (BSU.toString response) (pure $ successPayload Nothing))
                      else pure $ errorPayload "Incorrect arguments. Need a marble name, color, size and owner"

getMarblesByRange :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
getMarblesByRange s params =
    if Prelude.length params == 2
    then eitherToPbResponse <$> (runExceptT $ do
                                     sqi <- getStateByRange s (params !! 0) (params !! 1)
                                     resultBytes <- generateResultBytes sqi ""
                                     trace (show resultBytes) (pure $ successPayload Nothing))
    else pure $ errorPayload "Incorrect arguments. Need a start key and an end key"

-- -- TODO: include retrieval of next set of results using the returned bookmark (next TODO)
getMarblesByRangeWithPagination :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
getMarblesByRangeWithPagination s params =
    if Prelude.length params == 4
    then eitherToPbResponse
        <$> (runExceptT $ do
                 (sqi, metadata) <- getStateByRangeWithPagination s
                                                                  (params !! 0)
                                                                  (params !! 1)
                                                                  (read (unpack $ params !! 2) :: Int)
                                                                  (params !! 3)
                 resultBytes <- generateResultBytesForPagination (sqi, metadata) ""
                 trace (show resultBytes) (pure $ successPayload Nothing))
    else pure $ errorPayload "Incorrect arguments. Need start key, end key, pageSize and bookmark"

generateResultBytes :: StateQueryIterator -> Text -> ExceptT Error IO BSU.ByteString
generateResultBytes sqi text = ExceptT $ do
    hasNextBool <- hasNext sqi
    if hasNextBool
        then do
            eeKv <- runExceptT $ next sqi
            case eeKv of
                Left e -> pure $ Left e
                Right kv -> let makeKVString :: Pb.KV -> Text
                                makeKVString kv_ = pack "Key: " <> TL.toStrict (Pb.kvKey kv_) <> pack ", Value: "
                                    <> TSE.decodeUtf8 (kvValue kv_)
                            in
                                runExceptT $ generateResultBytes sqi (append text (makeKVString kv))
        else pure $ Right $ TSE.encodeUtf8 text

generateResultBytesForPagination
    :: (StateQueryIterator, Pb.QueryResponseMetadata) -> Text -> ExceptT Error IO BSU.ByteString
generateResultBytesForPagination (sqi, md) text = ExceptT $ do
    hasNextBool <- hasNext sqi
    if hasNextBool
        then do
            eeKv <- runExceptT $ next sqi
            case eeKv of
                Left e -> pure $ Left e
                Right kv -> let makeKVString :: Pb.KV -> Text
                                makeKVString kv_ = pack "Key: " <> TL.toStrict (Pb.kvKey kv_) <> pack ", Value: "
                                    <> TSE.decodeUtf8 (kvValue kv_)
                            in
                                runExceptT $ generateResultBytesForPagination (sqi, md) (append text (makeKVString kv))
        else pure $ Right $ TSE.encodeUtf8 text

parseMarble :: [Text] -> Marble
parseMarble params = Marble { objectType = "marble"
                            , name       = params !! 0
                            , color      = params !! 1
                            , size       = params !! 2
                            , owner      = params !! 3
                            }

marbleWithNewOwner :: Text -> Marble -> Marble
marbleWithNewOwner newOwner oldMarble =
    Marble { objectType = "marble"
           , name       = name oldMarble
           , color      = color oldMarble
           , size       = size oldMarble
           , owner      = newOwner
           }

eitherToPbResponse :: Show a => Either Error a -> Pb.Response
eitherToPbResponse (Right a) = successPayload $ Just $ BSU.fromString $ show a
eitherToPbResponse (Left err) = errorPayload $ pack $ show err
