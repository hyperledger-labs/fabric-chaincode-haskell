module Interfaces
  ( ChaincodeStubInterface(..)
  , StateQueryIteratorInterface(..)
  )
where

import           Data.ByteString
import           Data.Text
import           Data.Vector

import qualified Ledger.Queryresult.KvQueryResult
                                               as Pb
import qualified Google.Protobuf.Timestamp     as GooglePb
import qualified Peer.Proposal                 as Pb
import qualified Peer.ProposalResponse         as Pb
import qualified Peer.Chaincode                as Pb
import qualified Peer.ChaincodeShim            as Pb


import           Types



-- The ChaincodeStub type class defines the behaviour of the stub that is exposed to
-- the the Chaincode types to interact with the ledger.
class ChaincodeStubInterface ccs where
    getArgs :: ccs -> Vector ByteString
    getStringArgs :: ccs -> [Text]
    getFunctionAndParameters :: ccs -> Either Error (Text, [Text])
    getArgsSlice :: ccs -> Either Error ByteString
    getTxId :: ccs -> Text
    getChannelId :: ccs -> Text
    getSignedProposal :: ccs -> Maybe Pb.SignedProposal
    getCreator :: ccs -> Maybe ByteString
    getTransient :: ccs -> Maybe MapTextBytes
    getDecorations :: ccs -> MapTextBytes
    getBinding :: ccs -> Maybe MapTextBytes
    getTxTimestamp :: ccs -> Either Error GooglePb.Timestamp

    getState :: ccs -> Text -> IO (Either Error ByteString)
    putState :: ccs -> Text -> ByteString -> IO (Either Error ByteString)
    delState :: ccs -> Text -> IO (Either Error ByteString)

    -- setStateValidationParameter :: ccs -> String -> [ByteString] -> Maybe Error
    -- getStateValiationParameter :: ccs -> String -> Either Error [ByteString]
    getStateByRange :: ccs -> Text -> Text -> IO (Either Error StateQueryIterator)
    getStateByRangeWithPagination :: ccs -> Text -> Text -> Int -> Text -> IO (Either Error (StateQueryIterator, Pb.QueryResponseMetadata))
    
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
    -- setEvent :: ccs -> String -> ByteArray -> Maybe Error


-- The type class StateQueryIterator defines the behaviour of the types that expose functionalities
-- for iterating over a set of key/value pairs returned by a range query.
class StateQueryIteratorInterface sqi where
--     -- hasNext provides information on current status of the iterator and whether there are
--     -- more elements in the collection key-value pairs returned by the result.
    hasNext :: sqi -> IO Bool
--     -- close terminantes the iteration.
    close :: sqi -> IO (Maybe Error)
--     -- Provides the next key-value pair pointed by the iterator
--     TODO: Change this to an ExceptT type to make handling the next function
--     easier on the user chaincode side
    next :: sqi -> IO (Either Error Pb.KV)

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
