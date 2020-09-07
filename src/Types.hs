module Types where

import           Data.ByteString
import           Data.IORef
import           Data.Map
import           Data.Text
import qualified Data.Text.Lazy                   as TL
import qualified Data.Vector

import           Google.Protobuf.Timestamp        as Pb

import           Network.GRPC.HighLevel
import           Network.GRPC.HighLevel.Generated

import           Peer.ChaincodeShim               as Pb
import           Peer.Proposal                    as Pb
import           Peer.ProposalResponse            as Pb ( Response )

import           Proto3.Suite
import           Proto3.Wire.Decode

import           System.IO.Unsafe

data Error = GRPCError GRPCIOError | InvalidArgs | Error String | DecodeError ParseError
    deriving ( Eq, Show )

data ChaincodeStub = ChaincodeStub { initFn   :: DefaultChaincodeStub -> IO Pb.Response
                                   , invokeFn :: DefaultChaincodeStub -> IO Pb.Response
                                   }

-- Algebraic Type represeting the DefaultChaincodeStub.  This is the
-- one used to enable the chaincode to interact with ledger and chaincode
-- execution services of the peer. A istance of this type is created for
-- each of the chaincode invocations that are performed.
-- TODO: remove all these maybes when the stub is being created properly
data DefaultChaincodeStub = DefaultChaincodeStub   -- chaincode invocation arguments. serialised as arrays of bytes.
    { args           :: Data.Vector.Vector ByteString
      -- transaction identifier.
    , txId           :: Text
      -- channel identifier
    , channelId      :: Text
      -- timestamp of the transaction invocation
      -- txTimestamp :: Maybe Pb.Timestamp,
      -- bytes of the X.509 identity of the originator of the transaction.
    , creator        :: Maybe ByteString
      -- information about the signed proposal
    , signedProposal :: Maybe Pb.SignedProposal
    , proposal       :: Maybe Pb.Proposal
    , transient      :: Maybe MapTextBytes
    , binding        :: Maybe MapTextBytes
    , decorations    :: MapTextBytes
    , recvStream     :: StreamRecv ChaincodeMessage
    , sendStream     :: StreamSend ChaincodeMessage
    }

data StateQueryIterator =
    StateQueryIterator { sqiChaincodeStub :: DefaultChaincodeStub
                       , sqiChannelId     :: Text
                       , sqiTxId          :: Text
                       , sqiResponse      :: IORef Pb.QueryResponse
                       , sqiCurrentLoc    :: IORef Int
                       }
    deriving ( Show )

instance (Show a) => Show (IORef a) where
    show a = show (unsafePerformIO (readIORef a))

instance (Show DefaultChaincodeStub) where
    show ccs = "Chaincode stub { " ++ show (args ccs) ++ ", " ++ show (txId ccs) ++ ", " ++ show (channelId ccs) ++ ", "
        ++ show (creator ccs) ++ ", " ++ show (signedProposal ccs) ++ ", " ++ show (proposal ccs) ++ ", "
        ++ show (transient ccs) ++ ", " ++ show (binding ccs) ++ ", " ++ show (decorations ccs) ++ " }"

-- MapTextBytes is a synonym for the Map type whose keys are Text and values
type MapTextBytes = Map TL.Text ByteString