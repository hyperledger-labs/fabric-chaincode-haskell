{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Marbles where

import           GHC.Generics
import           Shim                           ( start
                                                , successPayload
                                                , errorPayload
                                                , ChaincodeStub(..)
                                                , ChaincodeStubInterface(..)
                                                , DefaultChaincodeStub
                                                , StateQueryIterator(..)
                                                , StateQueryIteratorInterface(..)
                                                , Error(..)
                                                )

import           Peer.ProposalResponse         as Pb

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                , append
                                                )
import qualified Data.Text.Encoding            as TSE
import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BSU
import qualified Data.ByteString.Lazy          as LBS

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , toEncoding
                                                , genericToEncoding
                                                , defaultOptions
                                                , encode
                                                , decode
                                                )

import           Debug.Trace

main :: IO ()
main = Shim.start chaincodeStub

chaincodeStub :: ChaincodeStub
chaincodeStub = ChaincodeStub { initFn = initFunc, invokeFn = invokeFunc }

data Marble = Marble {
    objectType :: Text,
    name :: Text,
    color :: Text,
    size :: Text,
    owner :: Text
} deriving (Generic, Show)

instance ToJSON Marble where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Marble

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s = 
  let e = getFunctionAndParameters s
  in
    case e of
      Left  _                              -> pure $ errorPayload ""
      Right ("initMarble"    , parameters) -> initMarble s parameters
      Right (fn              , _         ) -> pure
        $ errorPayload (pack ("Invoke did not find function: " ++ unpack fn))

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
  let e = getFunctionAndParameters s
  in
    case e of
      Left  _                              -> pure $ errorPayload ""
      Right ("initMarble"    , parameters) -> initMarble s parameters
      Right ("transferMarble", parameters) -> transferMarble s parameters
      -- Right ("transferMarbleBasedOnColor", parameters) ->
      --   transferMarbleBasedOnColor s parameters
      Right ("deleteMarble"  , parameters) -> deleteMarble s parameters
      Right ("readMarble"    , parameters) -> readMarble s parameters
      -- Right ("queryMarblesByOwner", parameters) ->
      --   queryMarblesByOwner s parameters
      -- Right ("queryMarbles", parameters) -> queryMarbles s parameters
      -- Right ("getHistoryForMarble", parameters) ->
      --   getHistoryForMarble s parameters
      Right ("getMarblesByRange", parameters) -> getMarblesByRange s parameters
      -- Right ("getMarblesByRangeWithPagination", parameters) ->
      --   getMarblesByRangeWithPagination s parameters
      -- Right ("queryMarblesWithPagination", parameters) ->
      --   queryMarblesWithPagination s parameters
      Right (fn              , _         ) -> pure
        $ errorPayload (pack ("Invoke did not find function: " ++ unpack fn))

-- TODO: implement CreateCompositeKey to index the marble by color
initMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
initMarble s params = if Prelude.length params == 4
  then do
    -- Check if marble already exists
    e <- getState s (head params)
    case e of
      Left  _        -> pure $ errorPayload "Failed to retrieve marble"
      Right response -> if BS.length response /= 0
        then pure $ errorPayload
          (pack ("This marble already exists: " ++ (unpack $ head params)))
        else
          -- marshal marble to JSON
          let marbleJSON = LBS.toStrict $ encode (parseMarble params)
          in  do
                ee <- putState s (head params) marbleJSON
                case ee of
                  Left  _ -> pure $ errorPayload "Failed to create marble"
                  Right _ -> pure $ successPayload Nothing
  else pure $ errorPayload
    "Incorrect arguments. Need a marble name, color, size and owner"

transferMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
transferMarble s params = if Prelude.length params == 2
  then do
    --   Check that the marble already exists
    e <- getState s (head params)
    case e of
      Left  _        -> pure $ errorPayload "Failed to get marble"
      Right response -> if BS.length response == 0
        then pure $ errorPayload "Marble not found"
        else
          -- Unmarshal the marble
          let maybeMarble = decode (LBS.fromStrict response) :: Maybe Marble
              marbleOwner = params !! 1
          in  case maybeMarble of
                Nothing -> pure $ errorPayload "Error decoding marble"
                Just oldMarble ->
                  -- Create a new marble instance with the new owner
                  let newMarble  = marbleWithNewOwner marbleOwner oldMarble
                      -- Marshal new marble to JSON
                      marbleJSON = LBS.toStrict $ encode newMarble
                  in  do
                        ee <- putState s (head params) marbleJSON
                        case ee of
                          Left _ ->
                            pure $ errorPayload "Failed to create marble"
                          Right _ -> pure $ successPayload Nothing
  else pure
    $ errorPayload "Incorrect arguments. Need a marble name and new owner"

-- TODO: Once indexing by color has been implemented, need to
-- get marble and also delete marble composite key
deleteMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
deleteMarble s params = if Prelude.length params == 1
  then do
    e <- delState s (head params)
    case e of
      Left  _ -> pure $ errorPayload "Failed to delete marble"
      Right _ -> pure $ successPayload Nothing
  else pure $ errorPayload "Incorrect arguments. Need a marble name"

readMarble :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
readMarble s params = if Prelude.length params == 1
  then do
    e <- getState s (head params)
    case e of
      Left  _ -> pure $ errorPayload "Failed to get marble"
      Right a -> trace (BSU.toString a) (pure $ successPayload Nothing)
  else pure $ errorPayload
    "Incorrect arguments. Need a marble name, color, size and owner"

getMarblesByRange :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
getMarblesByRange s params = if Prelude.length params == 2
  then do 
    e <- getStateByRange s (params !! 0) (params !! 1)
    case e of
      Left  _ -> pure $ errorPayload "Failed to get marbles"
      Right sqi -> do 
        resultBytes <- generateResultBytes sqi ""
        trace (show resultBytes) (pure $ successPayload Nothing) 
  else pure $ errorPayload "Incorrect arguments. Need a start key and an end key"

generateResultBytes :: StateQueryIterator -> Text -> IO (Either Error BSU.ByteString)
generateResultBytes sqi text = do 
  hasNextBool <- hasNext sqi
  if hasNextBool then do 
      eeKV <- next sqi
      -- TODO: We need to check that the Either Error KV returned from next 
      -- is correct and append the showable version of KVs instead of "abc".
      case eeKV of
        Right kv -> generateResultBytes sqi (append text $ pack $ show kv)
        Left e -> pure $ Left e
  else pure $ Right $ TSE.encodeUtf8 text

parseMarble :: [Text] -> Marble
parseMarble params = Marble { objectType = "marble"
                            , name       = params !! 0
                            , color      = params !! 1
                            , size       = params !! 2 
                            , owner      = params !! 3
                            }

marbleWithNewOwner :: Text -> Marble -> Marble
marbleWithNewOwner newOwner oldMarble = Marble { objectType = "marble"
                                               , name       = name oldMarble
                                               , color      = color oldMarble
                                               , size       = size oldMarble
                                               , owner      = newOwner
                                               }
