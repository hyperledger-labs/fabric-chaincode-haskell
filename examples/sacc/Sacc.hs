{-# LANGUAGE OverloadedStrings #-}

module Sacc where

import           Control.Monad.Except  ( runExceptT )

import           Data.ByteString.UTF8  as BSU ( fromString )
import           Data.Text             ( Text, pack )
import           Data.Text.Encoding    ( encodeUtf8 )

import           Peer.ProposalResponse as Pb

import           Shim                  ( ChaincodeStub(..)
                                       , ChaincodeStubInterface(..)
                                       , DefaultChaincodeStub
                                       , Error
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

initFunc :: DefaultChaincodeStub -> IO Pb.Response
initFunc s = let initArgs = getStringArgs s
             in
                 if Prelude.length initArgs == 2
                 then eitherToPbResponse <$> (runExceptT $ putState s (head initArgs) (encodeUtf8 $ initArgs !! 1))
                 else pure $ errorPayload "Incorrect arguments. Expecting a key and a value"

invokeFunc :: DefaultChaincodeStub -> IO Pb.Response
invokeFunc s = let e = getFunctionAndParameters s
               in
                   case e of
                       Left _ -> pure $ errorPayload ""
                       Right ("set", parameters) -> set s parameters
                       Right (_, parameters) -> get s parameters

set :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
set s params = if Prelude.length params == 2
               then eitherToPbResponse <$> (runExceptT $ putState s (head params) (encodeUtf8 $ params !! 1))
               else pure $ errorPayload "Incorrect arguments. Expecting a key and a value"

get :: DefaultChaincodeStub -> [Text] -> IO Pb.Response
get s params = if Prelude.length params == 1
               then eitherToPbResponse <$> (runExceptT $ getState s (head params))
               else pure $ errorPayload "Incorrect arguments. Expecting a key"

eitherToPbResponse :: Show a => Either Error a -> Pb.Response
eitherToPbResponse (Right a) = successPayload $ Just $ BSU.fromString $ show a
eitherToPbResponse (Left err) = errorPayload $ pack $ show err