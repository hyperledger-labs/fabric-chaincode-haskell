{-# LANGUAGE OverloadedStrings #-}
    
module Helper where

import           Data.Bifunctor                ( first )
import           Proto3.Suite                  as Suite
import           Common.Common                 as Pb
import           Peer.ChaincodeShim            as Pb
import           Peer.Chaincode                as Pb
import           Peer.Proposal                 as Pb
import           Peer.ProposalResponse         as Pb
import           Types                          ( Error(..)
                                                , ChaincodeStub(..)
                                                , MapTextBytes)
                                              

-- These are some helper functions to process the unmarshalling of different types
-- from the chaincode message in order to populate the stub
getChaincodeInput :: ChaincodeMessage -> Either Error Pb.ChaincodeInput
getChaincodeInput mes = first DecodeError $ Suite.fromByteString (chaincodeMessagePayload mes)

getProposal :: Pb.SignedProposal -> Either Error Pb.Proposal
getProposal signedProposal =
  first DecodeError $ Suite.fromByteString (signedProposalProposalBytes signedProposal)

getHeader :: Pb.Proposal -> Either Error Pb.Header
getHeader proposal = 
  first DecodeError $ Suite.fromByteString (proposalHeader proposal)

getChannelHeader :: Pb.Header -> Either Error Pb.ChannelHeader
getChannelHeader header = 
  first DecodeError $ Suite.fromByteString (headerChannelHeader header)

getChaincodeProposalPayload :: Pb.Proposal -> Either Error Pb.ChaincodeProposalPayload
getChaincodeProposalPayload proposal =
  first DecodeError $ Suite.fromByteString (proposalPayload proposal)

getSignatureHeader :: Pb.Header -> Either Error Pb.SignatureHeader
getSignatureHeader header =
  first DecodeError $ Suite.fromByteString (headerSignatureHeader header)

-- -- TODO: Use ChannelHeader and SignatureHeader to implement getBinding
createBinding :: Pb.Proposal -> Maybe MapTextBytes
createBinding _ = Nothing
