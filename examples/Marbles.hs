{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Example invocations:
-- peer chaincode invoke -n mycc -c '{"Args":["initMarble","marble1","red","large","Al"]}' -C myc
-- peer chaincode invoke -n mycc -c '{"Args":["readMarble","marble1"]}' -C myc
-- peer chaincode invoke -n mycc -c '{"Args":["deleteMarble","marble1"]}' -C myc
-- peer chaincode invoke -n mycc -c '{"Args":["transferMarble","marble1", "Nick"]}' -C myc

module Marbles where

import           GHC.Generics
import           Shim                           ( start
                                                , successPayload
                                                , errorPayload
                                                , ChaincodeStub(..)
                                                , ChaincodeStubInterface(..)
                                                , DefaultChaincodeStub
                                                )

import           Peer.ProposalResponse         as Pb

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
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
initFunc _ = pure $ successPayload Nothing


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
      -- Right ("getMarblesByRange", parameters) ->
      --   getMarblesByRange s parameters
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
