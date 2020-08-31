{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Fabcar where

import           GHC.Generics
import           Shim                           ( start
                                                , successPayload
                                                , errorPayload
                                                , ChaincodeStub(..)
                                                , ChaincodeStubInterface(..)
                                                , DefaultChaincodeStub
                                                )

import           Peer.ProposalResponse         as Pb

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , toEncoding
                                                , genericToEncoding
                                                , defaultOptions
                                                , encode
                                                , decode
                                                )

import           Data.Text                      ( Text )

import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BSU
import qualified Data.ByteString.Lazy          as LBS

import           Debug.Trace

main :: IO ()
main = Shim.start chaincodeStub

data Car = Car {
    make :: Text,
    model :: Text,
    colour :: Text,
    owner :: Text
} deriving (Generic, Show)

instance ToJSON Car where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Car

chaincodeStub :: ChaincodeStub
chaincodeStub = ChaincodeStub {initFn = initFunc, invokeFn = invokeFunc}

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc _ = pure $ successPayload Nothing

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
  let e = getFunctionAndParameters s
  in  case e of
        Left _ -> pure $ errorPayload "Failed to get function"
        Right ("initLedger"    , parameters) -> initLedger s parameters
        Right ("createCar"     , parameters) -> createCar s parameters
        Right ("queryCar"      , parameters) -> queryCar s parameters
        Right ("queryAllCars"  , parameters) -> queryAllCars s parameters
        Right ("changeCarOwner", parameters) -> changeCarOwner s parameters
        Right (_, _) -> pure $ errorPayload "No matching function found"

initLedger :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
initLedger s _ =
  let
    cars
      = [ Car
          { make   = "Toyota"
          , model  = "Prius"
          , colour = "blue"
          , owner  = "Tomoko"
          }
        , Car {make = "Ford", model = "Mustang", colour = "red", owner = "Brad"}
        , Car
          { make   = "Hyundai"
          , model  = "Tucson"
          , colour = "green"
          , owner  = "Jin Soo"
          }
        , Car
          { make   = "Volkswagen"
          , model  = "Passat"
          , colour = "yellow"
          , owner  = "Max"
          }
        , Car {make = "Tesla", model = "S", colour = "black", owner = "Adriana"}
        , Car
          { make   = "Peugeot"
          , model  = "205"
          , colour = "purple"
          , owner  = "Michel"
          }
        , Car
          { make   = "Chery"
          , model  = "S22L"
          , colour = "white"
          , owner  = "Aarav"
          }
        , Car
          { make   = "Fiat"
          , model  = "Punto"
          , colour = "violet"
          , owner  = "Pari"
          }
        , Car
          { make   = "Tata"
          , model  = "Nano"
          , colour = "indigo"
          , owner  = "Valeria"
          }
        , Car
          { make   = "Holden"
          , model  = "Barina"
          , colour = "brown"
          , owner  = "Shotaro"
          }
        ]
    keys =
      [ "CAR0"
      , "CAR1"
      , "CAR2"
      , "CAR3"
      , "CAR4"
      , "CAR5"
      , "CAR6"
      , "CAR7"
      , "CAR8"
      , "CAR9"
      , "CAR10"
      ]
  in
    createCars s keys cars

createCars :: DefaultChaincodeStub -> [Text] -> [Car] -> IO Pb.Response
createCars s keys cars = if length cars == 0
  then pure $ successPayload Nothing
  else
    let response = putState s (head keys) (LBS.toStrict $ encode $ head cars)
    in  do
          e <- response
          case e of
            Left  _ -> pure $ errorPayload "Failed to set asset"
            Right _ -> createCars s (tail keys) (tail cars)

createCar :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
createCar s params = if Prelude.length params == 5
  then
    let car = Car
          { make   = params !! 1
          , model  = params !! 2
          , colour = params !! 3
          , owner  = params !! 4
          }
        response = putState s (head params) (LBS.toStrict $ encode car)
    in  do
          e <- response
          case e of
            Left  _ -> pure $ errorPayload "Failed to set asset"
            Right _ -> pure $ successPayload Nothing
  else pure $ errorPayload "Incorrect number of arguments. Expecting 5"

queryCar :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
queryCar s params = if Prelude.length params == 1
  then
    let response = getState s (head params)
    in  do
          e <- response
          case e of
            Left  _        -> pure $ errorPayload "Failed to get asset"
            Right carBytes -> trace (BSU.toString carBytes)
                                    (pure $ successPayload $ Just carBytes)
  else pure $ errorPayload "Incorrect number of arguments. Expecting 1"

-- TODO: requires the getStateByRange stub function
queryAllCars :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
queryAllCars _ _ = pure $ errorPayload "Not yet implemented"
--     let 
--         startKey = "CAR0"
--         endKey = "CAR999"
--         response = getStateByRange s startKey endKey
--      in  do
--           e <- response
--           case e of
--             Left  _ -> pure $ errorPayload "Failed to get assets"
--             Right carsBytes -> trace (BSU.toString carsBytes) (pure $ successPayload $ Just carsBytes)

changeCarOwner :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
changeCarOwner s params = if Prelude.length params == 2
  then do
--   Check that the car already exists
    e <- getState s (head params)
    case e of
      Left  _        -> pure $ errorPayload "Failed to get car"
      Right response -> if BS.length response == 0
        then pure $ errorPayload "Car not found"
        else
-- Unmarshal the car
          let maybeCar = decode (LBS.fromStrict response) :: Maybe Car
              newOwner = params !! 1
          in  case maybeCar of
                Nothing -> pure $ errorPayload "Error decoding car"
                Just oldCar ->
                  let newCar  = carWithNewOwner oldCar newOwner
                      carJson = LBS.toStrict $ encode newCar
                  in  do
                        ee <- putState s (head params) carJson
                        case ee of
                          Left  _ -> pure $ errorPayload "Failed to create car"
                          Right _ -> pure $ successPayload Nothing
  else pure $ errorPayload "Incorrect arguments. Need a car name and new owner"

carWithNewOwner :: Car -> Text -> Car
carWithNewOwner oldCar newOwner = Car
  { make   = make oldCar
  , model  = model oldCar
  , colour = colour oldCar
  , owner  = newOwner
  }
