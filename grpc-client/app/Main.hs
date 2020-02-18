{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Shim                           ( start
                                                , successPayload
                                                , errorPayload
                                                , ChaincodeStub(..)
                                                , ChaincodeStubInterface(..)
                                                , DefaultChaincodeStub
                                                , Error
                                                )

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
chaincodeStub = ChaincodeStub { initFn = initFunc, invokeFn = invokeFunc }

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s =
  let initArgs = getArgs s
    in
      if Prelude.length initArgs == 3
        then
          let response = putState s (decodeUtf8 $ initArgs ! 1) (initArgs ! 2)
          in
            do
              e <- response :: IO (Either Error ByteString)
              case e of
                Left err -> trace ("Error putting state" ++ show err)
                                  (pure $ errorPayload "Error putting state")
                Right _ -> trace "Put state seemed to work!"
                                 (pure $ successPayload Nothing)
        else trace
          "Wrong arg number supplied for init"
        (pure $ errorPayload
          "Wrong number of arguments supplied for init. Two arguments needed"
        )

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
  let e = getFunctionAndParameters s
  in  case e of
        Left _ -> pure $ errorPayload "Error getting function and parameters"
        Right ("get", parameters) -> fetchState s parameters
        Right ("put", parameters) -> createState s parameters
        Right ("del", parameters) -> deleteState s parameters
        Right ("getArgSlice", _) -> getArgSlice s
        Right (_, _) -> pure $ errorPayload "No function with that name found"

fetchState :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
fetchState s params = if Prelude.length params == 1
  then
    let response = getState s (head params)
    in  do
          e <- response
          case e of
            Left err -> trace ("Error getting state" ++ show err)
                              (pure $ errorPayload "Error getting state")
            Right a -> trace (BSU.toString a) (pure $ successPayload Nothing)
  else trace
    "Wrong number of arguments supplied for get. One argument needed"
    (pure $ errorPayload
      "Wrong number of arguments supplied for get. One argument needed"
    )

createState :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
createState s params = if Prelude.length params == 2
  then
    let response = putState s (head params) (encodeUtf8 $ head $ tail params)
    in
      do
        e <- response :: IO (Either Error ByteString)
        case e of
          Left err -> trace ("Error putting state" ++ show err)
                            (pure $ errorPayload "Error putting state")
          Right _ ->
            trace "Put state seemed to work!" (pure $ successPayload Nothing)
  else trace
    "Wrong number of arguments supplied for put. Two arguments needed"
    (pure $ errorPayload
      "Wrong number of arguments supplied for put. Two arguments needed"
    )

getArgSlice :: DefaultChaincodeStub -> IO Pb.Response
getArgSlice s =
  let argSlice = getArgsSlice s
  in  case argSlice of
        Left err -> trace ("Error in getArgsSlice " ++ show err)
                          (pure $ errorPayload "Error getting argslice")
        Right bs -> trace ("getArgSlice bytestring: " ++ toString bs)
                          (pure $ successPayload Nothing)

deleteState :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
deleteState s params = if Prelude.length params == 1
  then
    let response = delState s (head params)
    in  do
          e <- response :: IO (Either Error ByteString)
          case e of
            Left err -> trace ("Error deleting state" ++ show err)
                              (pure $ errorPayload "Error deleting state")
            Right _ -> trace "State deleted!" (pure $ successPayload Nothing)
  else trace
    "Wrong number of arguments supplied for del. One argument needed"
    (pure $ errorPayload
      "Wrong number of arguments supplied for del. One argument needed"
    )
