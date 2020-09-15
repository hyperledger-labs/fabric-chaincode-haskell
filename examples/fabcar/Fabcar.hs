{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fabcar where

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
import           Data.Text                        ( Text, append, pack )
import qualified Data.Text.Encoding               as TSE
import qualified Data.Text.Lazy                   as TL

import           GHC.Generics

import           Ledger.Queryresult.KvQueryResult as Pb

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

data Car = Car { make   :: Text
               , model  :: Text
               , colour :: Text
               , owner  :: Text
               }
    deriving ( Generic, Show )

instance ToJSON Car where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Car

chaincodeStub :: ChaincodeStub
chaincodeStub = ChaincodeStub { initFn   = initFunc
                              , invokeFn = invokeFunc
                              }

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc _ = pure $ successPayload Nothing

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
    let e = getFunctionAndParameters s
    in
        case e of
            Left _ -> pure $ errorPayload "Failed to get function"
            Right ("initLedger", parameters) -> initLedger s parameters
            Right ("createCar", parameters) -> createCar s parameters
            Right ("queryCar", parameters) -> queryCar s parameters
            Right ("queryAllCars", parameters) -> queryAllCars s parameters
            Right ("changeCarOwner", parameters) -> changeCarOwner s parameters
            Right (_, _) -> pure $ errorPayload "No matching function found"

initLedger :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
initLedger s _ =
    let cars =
            [ Car { make   = "Toyota"
                  , model  = "Prius"
                  , colour = "blue"
                  , owner  = "Tomoko"
                  }
            , Car { make   = "Ford"
                  , model  = "Mustang"
                  , colour = "red"
                  , owner  = "Brad"
                  }
            , Car { make   = "Hyundai"
                  , model  = "Tucson"
                  , colour = "green"
                  , owner  = "Jin Soo"
                  }
            , Car { make   = "Volkswagen"
                  , model  = "Passat"
                  , colour = "yellow"
                  , owner  = "Max"
                  }
            , Car { make   = "Tesla"
                  , model  = "S"
                  , colour = "black"
                  , owner  = "Adriana"
                  }
            , Car { make   = "Peugeot"
                  , model  = "205"
                  , colour = "purple"
                  , owner  = "Michel"
                  }
            , Car { make   = "Chery"
                  , model  = "S22L"
                  , colour = "white"
                  , owner  = "Aarav"
                  }
            , Car { make   = "Fiat"
                  , model  = "Punto"
                  , colour = "violet"
                  , owner  = "Pari"
                  }
            , Car { make   = "Tata"
                  , model  = "Nano"
                  , colour = "indigo"
                  , owner  = "Valeria"
                  }
            , Car { make   = "Holden"
                  , model  = "Barina"
                  , colour = "brown"
                  , owner  = "Shotaro"
                  }
            ]
        keys = [ "CAR0", "CAR1", "CAR2", "CAR3", "CAR4", "CAR5", "CAR6", "CAR7", "CAR8", "CAR9", "CAR10" ]
    in
        createCars s keys cars

createCars :: DefaultChaincodeStub -> [Text] -> [Car] -> IO Pb.Response
createCars s keys cars =
    if length cars == 0
    then pure $ successPayload Nothing
    else do
        eitherErrBS <- runExceptT (putState s (head keys) (LBS.toStrict $ encode $ head cars))
        case eitherErrBS of
            Left e -> pure $ errorPayload $ pack $ show e
            Right _ -> createCars s (tail keys) (tail cars)

createCar :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
createCar s params =
    if Prelude.length params == 5
    then let car = Car { make   = params !! 1
                       , model  = params !! 2
                       , colour = params !! 3
                       , owner  = params !! 4
                       }
         in
             eitherToPbResponse <$> runExceptT (putState s (head params) (LBS.toStrict $ encode car))
    else pure $ errorPayload "Incorrect number of arguments. Expecting 5"

queryCar :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
queryCar s params = if Prelude.length params == 1
                    then eitherToPbResponse <$> runExceptT (getState s (head params))
                    else pure $ errorPayload "Incorrect number of arguments. Expecting 1"

queryAllCars :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
queryAllCars s params =
    if Prelude.length params == 0
    then eitherToPbResponse <$> (runExceptT $ do
                                     sqi <- getStateByRange s "" ""
                                     resultBytes <- generateResultBytes sqi ""
                                     pure $ successPayload (Just resultBytes))
    else pure $ errorPayload "Incorrect number of arguments. Should be no arguments"

changeCarOwner :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
changeCarOwner s params =
    if Prelude.length params == 2
    then eitherToPbResponse
        <$> (runExceptT $ do
                 --   Check that the car already exists
                 response <- getState s (head params)
                 if BS.length response == 0
                     then throwError $ Error "Car not found"
                     else 
                         -- Unmarshal the car
                         let maybeCar = decode (LBS.fromStrict response) :: Maybe Car
                             newOwner = params !! 1
                         in
                             case maybeCar of
                                 Nothing -> throwError $ Error "Error decoding car"
                                 Just oldCar -> let newCar = carWithNewOwner oldCar newOwner
                                                    carJson = LBS.toStrict $ encode newCar
                                                in
                                                    putState s (head params) carJson)
    else pure $ errorPayload "Incorrect arguments. Need a car name and new owner"

carWithNewOwner :: Car -> Text -> Car
carWithNewOwner oldCar newOwner =
    Car { make   = make oldCar
        , model  = model oldCar
        , colour = colour oldCar
        , owner  = newOwner
        }

eitherToPbResponse :: Show a => Either Error a -> Pb.Response
eitherToPbResponse (Right a) = successPayload $ Just $ BSU.fromString $ show a
eitherToPbResponse (Left err) = errorPayload $ pack $ show err

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