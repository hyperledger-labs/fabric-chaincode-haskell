{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Shim
import           Stub
import           Interfaces
import           Messages
import           Error

import           Peer.ProposalResponse         as Pb

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.ByteString.UTF8          as BSU
import           Data.Vector                    ( (!) )

import           Debug.Trace

main :: IO ()
main = Shim.start chaincodeStub

chaincodeStub = ChaincodeStub {initFn = initFunc, invokeFn = invokeFunc}

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s =
  let response = putState s (decodeUtf8 $ getArgs s ! 1) (getArgs s ! 2)
  in  do
        e <- response :: IO (Either Error ByteString)
        case e of
          Left err ->
            trace ("Error putting state" ++ (show err)) (pure successPayload)
          Right _ -> trace "Put state seemed to work!" (pure successPayload)

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
  let e = getFunctionAndParameters s
  in  case e of
        Left  _                   -> pure failPayload
        Right ("get", parameters) -> fetchState s parameters
        Right ("put", parameters) -> createState s parameters

-- TODO check that args length is 1
fetchState :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
fetchState s args =
  let response = getState s (head args)
  in  do
        e <- response
        case e of
          Left err ->
            trace ("Error getting state" ++ (show err)) (pure successPayload)
          Right a -> trace (BSU.toString a) (pure successPayload)

-- TODO check that args length is > 2
createState :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
createState s args =
  let response = putState s (head args) (head $ tail args)
  in  do
        e <- response :: IO (Either Error ByteString)
        case e of
          Left err ->
            trace ("Error putting state" ++ (show err)) (pure successPayload)
          Right _ -> trace "Put state seemed to work!" (pure successPayload)
