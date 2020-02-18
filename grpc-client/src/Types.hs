module Types where

import           Data.ByteString
import           Data.Map
import           Data.Vector
import           Data.Text

import           Network.GRPC.HighLevel.Generated
import           Proto3.Suite
import           Network.GRPC.HighLevel

import           Peer.ChaincodeShim
import           Google.Protobuf.Timestamp     as Pb
import           Peer.Proposal                 as Pb
import           Peer.ProposalResponse         as Pb

data Error = GRPCError GRPCIOError
    | InvalidArgs
    | Error String
   deriving (Eq, Show)

data ChaincodeStub = ChaincodeStub {
   initFn :: DefaultChaincodeStub -> IO Pb.Response,
   invokeFn :: DefaultChaincodeStub -> IO Pb.Response
}

-- Algebraic Type represeting the DefaultChaincodeStub.  This is the
-- one used to enable the chaincode to interact with ledger and chaincode
-- execution services of the peer. A istance of this type is created for
-- each of the chaincode invocations that are performed.
-- TODO: remove all these maybes when the stub is being created properly
data DefaultChaincodeStub = DefaultChaincodeStub {
    -- chaincode invocation arguments. serialised as arrays of bytes.
    args :: Vector ByteString,
    -- -- name of the function being invoked.
    -- function :: Maybe Text,
    -- -- arguments of the function idenfied by the chaincode invocation.
    -- parameters :: Maybe [String],
    -- transaction identifier.
    txId :: Text,
    -- channel identifier
    channelId:: Text,
    -- timestamp of the transaction invocation
    txTimestamp :: Maybe Pb.Timestamp,
    -- bytes of the X.509 identity of the originator of the transaction.
    creator :: Maybe ByteString,
    -- information about the signed proposalgit
    signedProposal :: Maybe Pb.SignedProposal,
    transient :: Maybe MapStringBytes,
    binding :: Maybe MapStringBytes,
    decorations :: Maybe MapStringBytes,
    recvStream :: StreamRecv ChaincodeMessage,
    sendStream :: StreamSend ChaincodeMessage
}

-- MapStringBytes is a synonym for the Map type whose keys are String and values
type MapStringBytes = Map String ByteString
