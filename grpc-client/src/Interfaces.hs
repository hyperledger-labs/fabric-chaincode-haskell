module Interfaces where

import Data.Map
import Data.ByteString

import qualified Ledger.Queryresult.KvQueryResult as Pb
import qualified Google.Protobuf.Timestamp as GooglePb
import qualified Peer.Proposal as Pb
import qualified Peer.ProposalResponse as Pb
import qualified Peer.Chaincode as Pb

-- Error data simply contains a string that specifies the error that has occurred.
data Error = Error { message :: String } deriving Show

-- MapStringBytes is a synonym for the Map type whose keys are String and values
--
type MapStringBytes = Map String ByteString



-- The Chaincode type classs defines the constraints on the behaviour of types that
-- implement its methods. The fabric shim will un the transactions by calling these
-- functions on an instance type of this type class.
-- class Chaincode cc where
--     -- Init is called during the invocation of the Instantiate transaction after
--     -- the chaincode container has established for the first time, allowing the
--     -- chaincode to initialise itself.
--     init ::  cc -> ChaincodeStub -> Pb.Response
--     -- Invoke is called to update or query the ledger in a proposal transaction.
--     -- Updatd state variables are not committed to the ledger until the transaction
--     -- is committed.
--     invoke  :: cc -> ChaincodeStub -> Pb.Response

-- The ChaincodeStuv type class defines the behaviour of the stub that is exposed to
-- the the Chaincode types to interact with the ledger.
class ChaincodeStubI ccs where
    -- getArgs :: ccs -> [ByteString]
    -- getStringArgs :: ccs -> [String]
    -- getFunctionAndParameters :: ccs -> (String, [String])
    -- getArgsSlice :: ccs -> Either Error ByteString
    getTxId :: ccs -> String
    -- getChannelId :: ccs -> String
    -- invokeChaincode :: ccs -> String -> [ByteArray] -> String -> Pb.Response
    -- getState :: ccs -> String -> Either Error ByteString
    -- putState :: ccs -> String -> ByteString -> Maybe Error

    -- delState :: ccs -> String -> Maybe Error
    -- setStateValidationParameter :: ccs -> String -> [ByteString] -> Maybe Error
    -- getStateValiationParameter :: ccs -> String -> Either Error [ByteString]
    -- getStateByRange :: ccs -> String -> String -> Either Error StateQueryIterator
    -- getStateByRangeWithPagination :: ccs -> String -> String -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- getStateByPartialCompositeKey :: ccs -> String -> [String] -> Either Error StateQueryIterator
    -- getStateByPartialCompositeKeyWithPagination :: ccs -> String -> [String] -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- createCompositeKey :: ccs -> String -> [String] -> Either Error String
    -- splitCompositeKey :: ccs -> String -> Either Error (String, [String])
    -- getQueryResult :: ccs -> String -> Either Error StateQueryIterator
    -- getQueryResultWithPagination :: ccs -> String -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- getHistoryForKey :: ccs -> Either Error HistoryQueryIterator
    -- getPrivateData :: ccs -> String -> String -> Either Error ByteString
    -- getPrivateDataHash :: ccs -> String -> String -> Either Error ByteString
    -- putPrivateData :: ccs -> String -> String -> ByteString -> Maybe Error
    -- delPrivateData :: ccs -> String -> String -> Maybe Error
    -- setPrivateDataValidationParameter :: ccs -> String -> String -> ByteString -> Maybe Error
    -- getPrivateDataValidationParameter :: ccs -> String -> String -> Either Error ByteString
    -- getPrivateDataByRange :: ccs -> String -> String -> String -> Either Error StateQueryIterator
    -- getPrivateDataByPartialCompositeKey :: ccs -> String -> String -> [String] -> Either Error StateQueryIterator
    -- getPrivateDataQueryResult :: ccs -> String -> String -> Either Error StateQueryIterator
    -- getCreator :: ccs -> Either Error ByteString
    -- getTransient :: ccs -> Either Error MapStringBytes
    -- getBinding :: ccs -> Either Error MapStringBytes
    -- getDecorations :: ccs -> MapStringBytes
    -- getSignedProposal :: ccs -> Either Error Pb.SignedProposal
    -- getTxTimestamp :: ccs -> Either Error GooglePb.Timestamp
    -- setEvent :: ccs -> String -> ByteArray -> Maybe Error





-- The type class StateQueryIterator defines the behaviour of the types that expose functionalities
-- for iterating over a set of key/value pairs returned by a range query.
-- class StateQueryIterator sqi where
--     -- hasNext provides information on current status of the iterator and whether there are
--     -- more elements in the collection key-value pairs returned by the result.
--     hasNext :: sqi -> Bool
--     -- close terminantes the iteration.
--     close :: sqi -> Maybe Error
--     -- Provides the next key-value pair pointed by the iterator
--     next :: sqi -> Either Error Pb.KV

-- The type class HistoryQueryIterator defines the behaviour of the types that expose functionalities
-- for iteratogin over a set of key modifications that are associated to the history of a key.
-- class HistoryQueryIterator hqi where
--     -- hasNext provides information on current status of the iterator and whether there are
--     -- more elements in the collection key modifications returned by the result.
--     hasNext :: sqi -> Bool
--     -- close terminantes the iteration.
--     close :: sqi -> Maybe Error
--     -- Provides the next key modification pointed by the iterator
--     next :: sqi -> Either Error Pb.KeyModification
