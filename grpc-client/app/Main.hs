{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Shim
import           Stub
import           Interfaces
import           Messages
import           Error

import           Peer.ProposalResponse         as Pb

import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.ByteString.UTF8          as BSU
import           Data.Vector                    ( (!) )

import           Debug.Trace

main :: IO ()
main = Shim.start chaincodeStub

chaincodeStub :: ChaincodeStub
chaincodeStub = ChaincodeStub {initFn = initFunc, invokeFn = invokeFunc}

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s
  = let initArgs = getArgs s
    in
      if (Prelude.length initArgs == 3)
        then
          let response = putState s (decodeUtf8 $ initArgs ! 1) (initArgs ! 2)
          in  do
                e <- response :: IO (Either Error ByteString)
                case e of
                  Left err -> trace
                    ("Error putting state" ++ (show err))
                    (pure $ failPayload "Error putting state")
                  Right _ ->
                    trace "Put state seemed to work!" (pure successPayload)
        else trace
          "Wrong arg number supplied for init"
          ( pure
          $ failPayload
              "Wrong number of arguments supplied for init. Two arguments needed"
          )

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
  let e = getFunctionAndParameters s
  in  case e of
        Left _ -> pure $ failPayload "Error getting function and parameters"
        Right ("get", parameters) -> fetchState s parameters
        Right ("put", parameters) -> createState s parameters
        Right ("getArgSlice", _) -> getArgSlice s
        Right (_, _) -> pure $ failPayload "No function with that name found"

fetchState :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
fetchState s params = if (Prelude.length params == 1)
  then
    let response = getState s (head params)
    in  do
          e <- response
          case e of
            Left err -> trace ("Error getting state" ++ (show err))
                              (pure $ failPayload "Error getting state")
            Right a -> trace (BSU.toString a) (pure successPayload)
  else trace
    "Wrong number of arguments supplied for get. One argument needed"
    (pure $ failPayload
      "Wrong number of arguments supplied for get. One argument needed"
    )

createState :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
createState s params = if (Prelude.length params == 2)
  then
    let response = putState s (head params) (encodeUtf8 $ head $ tail params)
    in  do
          e <- response :: IO (Either Error ByteString)
          case e of
            Left err -> trace ("Error putting state" ++ (show err))
                              (pure $ failPayload "Error putting state")
            Right _ -> trace "Put state seemed to work!" (pure successPayload)
  else trace
    "Wrong number of arguments supplied for put. Two arguments needed"
    (pure $ failPayload
      "Wrong number of arguments supplied for put. Two arguments needed"
    )

getArgSlice :: DefaultChaincodeStub -> IO Pb.Response
getArgSlice s =
  let argSlice = getArgsSlice s
  in  case argSlice of
        Left err -> trace ("Error in getArgsSlice " ++ (show err))
                          (pure $ failPayload "Error getting argslice")
        Right bs -> trace ("getArgSlice bytestring: " ++ (toString bs))
                          (pure $ successPayload)
