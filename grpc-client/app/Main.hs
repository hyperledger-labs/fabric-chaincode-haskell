module Main where

import Shim
import Stub
import Interfaces

import Peer.ProposalResponse as Pb

import Debug.Trace

main :: IO ()
main = Shim.start chaincodeStub

chaincodeStub = ChaincodeStub {
    initFn = initFunc,
    invokeFn = invokeFunc
}

initFunc :: DefaultChaincodeStub -> Pb.Response
initFunc s = trace (getTxId s) successPayload

invokeFunc :: DefaultChaincodeStub -> Pb.Response
invokeFunc s = trace (getTxId s) successPayload
