{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

module Main where

import Shim

import Peer.ProposalResponse as Pb


main :: IO ()
main = start initFn

-- data Stub = Stub Int
initFn :: Int -> Pb.Response
initFn _ = initPayload
