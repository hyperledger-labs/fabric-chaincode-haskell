{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Shim
import           Stub
import           Interfaces
import           Messages
import           Error

import           Peer.ProposalResponse         as Pb

import           Data.Text.Lazy                 ( unpack )
import           Data.ByteString.UTF8          as BSU

import           Debug.Trace

main :: IO ()
main = Shim.start chaincodeStub

chaincodeStub = ChaincodeStub {initFn = initFunc, invokeFn = invokeFunc}

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s =
  let response = putState s "a" (BSU.fromString "b")
  in  do
        e <- response :: IO (Either Error ByteString)
        case e of
          Left err ->
            trace ("Error putting state" ++ (show err)) (pure successPayload)
          Right _ -> trace "Put state seemed to work!" (pure successPayload)

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
  let response = getState s "c"
  in  do
        e <- response
        case e of
          Left err ->
            trace ("Error getting state" ++ (show err)) (pure successPayload)
          Right a -> trace (BSU.toString a) (pure successPayload)
