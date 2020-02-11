{-# LANGUAGE OverloadedStrings #-}

module Main where

import Shim
import Stub
import Interfaces

import Peer.ProposalResponse as Pb

import Debug.Trace


main :: IO ()
main = start initFn

initFn :: DefaultChaincodeStub -> Pb.Response
initFn s = trace (getTxId s) initPayload
