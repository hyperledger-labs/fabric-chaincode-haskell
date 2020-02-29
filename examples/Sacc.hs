{-# LANGUAGE OverloadedStrings #-}

module Sacc where

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
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.ByteString.UTF8          as BSU
import           Debug.Trace

main :: IO ()
main = Shim.start chaincodeStub

chaincodeStub :: ChaincodeStub
chaincodeStub = ChaincodeStub { initFn = initFunc, invokeFn = invokeFunc }

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s =
  let initArgs = getStringArgs s
  in  if Prelude.length initArgs == 2
        then
          let response =
                putState s (head initArgs) (encodeUtf8 $ initArgs !! 1)
          in  do
                e <- response :: IO (Either Error ByteString)
                case e of
                  Left  _ -> pure $ errorPayload "Failed to create asset"
                  Right _ -> pure $ successPayload Nothing
        else pure
          $ errorPayload "Incorrect arguments. Expecting a key and a value"

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s =
  let e = getFunctionAndParameters s
  in  case e of
        Left  _                   -> pure $ errorPayload ""
        Right ("set", parameters) -> set s parameters
        Right (_    , parameters) -> get s parameters

set :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
set s params = if Prelude.length params == 2
  then
    let response = putState s (head params) (encodeUtf8 $ params !! 1)
    in  do
          e <- response :: IO (Either Error ByteString)
          case e of
            Left  _ -> pure $ errorPayload "Failed to set asset"
            Right _ -> pure $ successPayload Nothing
  else pure $ errorPayload "Incorrect arguments. Expecting a key and a value"

get :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
get s params = if Prelude.length params == 1
  then
    let response = getState s (head params)
    in  do
          e <- response
          case e of
            Left  _ -> pure $ errorPayload "Failed to get asset"
            Right a -> trace (BSU.toString a) (pure $ successPayload Nothing)
  else pure $ errorPayload "Incorrect arguments. Expecting a key"
