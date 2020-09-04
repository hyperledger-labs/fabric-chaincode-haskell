module Interfaces ( ChaincodeStubInterface(..), StateQueryIteratorInterface(..) ) where

import           Control.Monad.Except             ( ExceptT(..) )

import           Data.ByteString
import           Data.Text
import           Data.Vector

import qualified Google.Protobuf.Timestamp        as GooglePb

import qualified Ledger.Queryresult.KvQueryResult as Pb

import qualified Peer.Chaincode                   as Pb
import qualified Peer.ChaincodeShim               as Pb
import qualified Peer.Proposal                    as Pb
import qualified Peer.ProposalResponse            as Pb

import           Types

-- The ChaincodeStub type class defines the behaviour of the stub that is exposed to
-- the the Chaincode types to interact with the ledger.
class ChaincodeStubInterface ccs where
    -- GetArgs returns the arguments intended for the chaincode Init and Invoke
    -- as an array of byte arrays.
    getArgs :: ccs -> Vector ByteString

    -- GetStringArgs returns the arguments intended for the chaincode Init and
    -- Invoke as a string array. Only use GetStringArgs if the client passes
    -- arguments intended to be used as strings.
    getStringArgs :: ccs -> [Text]

    -- GetFunctionAndParameters returns the first argument as the function
    -- name and the rest of the arguments as parameters in a string array.
    -- Only use GetFunctionAndParameters if the client passes arguments intended
    -- to be used as strings.
    getFunctionAndParameters :: ccs -> Either Error (Text, [Text])

    -- GetArgsSlice returns the arguments intended for the chaincode Init and
    -- Invoke as a byte array
    getArgsSlice :: ccs -> Either Error ByteString

    -- GetTxID returns the tx_id of the transaction proposal, which is unique per
    -- transaction and per client. See
    -- https:--godoc.org/github.com/hyperledger/fabric-protos-go/common#ChannelHeader
    -- for further details.
    getTxId :: ccs -> Text

    -- GetChannelID returns the channel the proposal is sent to for chaincode to process.
    -- This would be the channel_id of the transaction proposal (see
    -- https:--godoc.org/github.com/hyperledger/fabric-protos-go/common#ChannelHeader )
    -- except where the chaincode is calling another on a different channel.
    getChannelId :: ccs -> Text

    -- GetState returns the value of the specified `key` from the
    -- ledger. Note that GetState doesn't read data from the writeset, which
    -- has not been committed to the ledger. In other words, GetState doesn't
    -- consider data modified by PutState that has not been committed.
    -- If the key does not exist in the state database, (nil, nil) is returned.
    getState :: ccs -> Text -> ExceptT Error IO ByteString

    -- PutState puts the specified `key` and `value` into the transaction's
    -- writeset as a data-write proposal. PutState doesn't effect the ledger
    -- until the transaction is validated and successfully committed.
    -- Simple keys must not be an empty string and must not start with a
    -- null character (0x00) in order to avoid range query collisions with
    -- composite keys, which internally get prefixed with 0x00 as composite
    -- key namespace. In addition, if using CouchDB, keys can only contain
    -- valid UTF-8 strings and cannot begin with an underscore ("_").
    putState :: ccs -> Text -> ByteString -> ExceptT Error IO ByteString

    -- DelState records the specified `key` to be deleted in the writeset of
    -- the transaction proposal. The `key` and its value will be deleted from
    -- the ledger when the transaction is validated and successfully committed.
    delState :: ccs -> Text -> ExceptT Error IO ByteString

    -- SetStateValidationParameter sets the key-level endorsement policy for `key`.
    -- setStateValidationParameter :: ccs -> String -> [ByteString] -> Maybe Error
    -- GetStateValidationParameter retrieves the key-level endorsement policy
    -- for `key`. Note that this will introduce a read dependency on `key` in
    -- the transaction's readset.
    -- getStateValiationParameter :: ccs -> String -> Either Error [ByteString]
    -- GetStateByRange returns a range iterator over a set of keys in the
    -- ledger. The iterator can be used to iterate over all keys
    -- between the startKey (inclusive) and endKey (exclusive).
    -- However, if the number of keys between startKey and endKey is greater than the
    -- totalQueryLimit (defined in core.yaml), this iterator cannot be used
    -- to fetch all keys (results will be capped by the totalQueryLimit).
    -- The keys are returned by the iterator in lexical order. Note
    -- that startKey and endKey can be empty string, which implies unbounded range
    -- query on start or end.
    -- Call Close() on the returned StateQueryIteratorInterface object when done.
    -- The query is re-executed during validation phase to ensure result set
    -- has not changed since transaction endorsement (phantom reads detected).
    getStateByRange :: ccs -> Text -> Text -> ExceptT Error IO StateQueryIterator

    -- GetStateByRangeWithPagination returns a range iterator over a set of keys in the
    -- ledger. The iterator can be used to fetch keys between the startKey (inclusive)
    -- and endKey (exclusive).
    -- When an empty string is passed as a value to the bookmark argument, the returned
    -- iterator can be used to fetch the first `pageSize` keys between the startKey
    -- (inclusive) and endKey (exclusive).
    -- When the bookmark is a non-emptry string, the iterator can be used to fetch
    -- the first `pageSize` keys between the bookmark (inclusive) and endKey (exclusive).
    -- Note that only the bookmark present in a prior page of query results (ResponseMetadata)
    -- can be used as a value to the bookmark argument. Otherwise, an empty string must
    -- be passed as bookmark.
    -- The keys are returned by the iterator in lexical order. Note
    -- that startKey and endKey can be empty string, which implies unbounded range
    -- query on start or end.
    -- Call Close() on the returned StateQueryIteratorInterface object when done.
    -- This call is only supported in a read only transaction.
    getStateByRangeWithPagination
        :: ccs -> Text -> Text -> Int -> Text -> ExceptT Error IO (StateQueryIterator, Pb.QueryResponseMetadata)

    -- GetStateByPartialCompositeKey queries the state in the ledger based on
    -- a given partial composite key. This function returns an iterator
    -- which can be used to iterate over all composite keys whose prefix matches
    -- the given partial composite key. However, if the number of matching composite
    -- keys is greater than the totalQueryLimit (defined in core.yaml), this iterator
    -- cannot be used to fetch all matching keys (results will be limited by the totalQueryLimit).
    -- The `objectType` and attributes are expected to have only valid utf8 strings and
    -- should not contain U+0000 (nil byte) and U+10FFFF (biggest and unallocated code point).
    -- See related functions SplitCompositeKey and CreateCompositeKey.
    -- Call Close() on the returned StateQueryIteratorInterface object when done.
    -- The query is re-executed during validation phase to ensure result set
    -- has not changed since transaction endorsement (phantom reads detected).
    -- getStateByPartialCompositeKey :: ccs -> String -> [String] -> Either Error StateQueryIterator
    -- GetStateByPartialCompositeKeyWithPagination queries the state in the ledger based on
    -- a given partial composite key. This function returns an iterator
    -- which can be used to iterate over the composite keys whose
    -- prefix matches the given partial composite key.
    -- When an empty string is passed as a value to the bookmark argument, the returned
    -- iterator can be used to fetch the first `pageSize` composite keys whose prefix
    -- matches the given partial composite key.
    -- When the bookmark is a non-emptry string, the iterator can be used to fetch
    -- the first `pageSize` keys between the bookmark (inclusive) and the last matching
    -- composite key.
    -- Note that only the bookmark present in a prior page of query result (ResponseMetadata)
    -- can be used as a value to the bookmark argument. Otherwise, an empty string must
    -- be passed as bookmark.
    -- The `objectType` and attributes are expected to have only valid utf8 strings
    -- and should not contain U+0000 (nil byte) and U+10FFFF (biggest and unallocated
    -- code point). See related functions SplitCompositeKey and CreateCompositeKey.
    -- Call Close() on the returned StateQueryIteratorInterface object when done.
    -- This call is only supported in a read only transaction.
    -- getStateByPartialCompositeKeyWithPagination :: ccs -> String -> [String] -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- CreateCompositeKey combines the given `attributes` to form a composite
    -- key. The objectType and attributes are expected to have only valid utf8
    -- strings and should not contain U+0000 (nil byte) and U+10FFFF
    -- (biggest and unallocated code point).
    -- The resulting composite key can be used as the key in PutState().
    -- createCompositeKey :: ccs -> String -> [String] -> Either Error String
    -- SplitCompositeKey splits the specified key into attributes on which the
    -- composite key was formed. Composite keys found during range queries
    -- or partial composite key queries can therefore be split into their
    -- composite parts.
    -- splitCompositeKey :: ccs -> String -> Either Error (String, [String])
    -- GetQueryResult performs a "rich" query against a state database. It is
    -- only supported for state databases that support rich query,
    -- e.g.CouchDB. The query string is in the native syntax
    -- of the underlying state database. An iterator is returned
    -- which can be used to iterate over all keys in the query result set.
    -- However, if the number of keys in the query result set is greater than the
    -- totalQueryLimit (defined in core.yaml), this iterator cannot be used
    -- to fetch all keys in the query result set (results will be limited by
    -- the totalQueryLimit).
    -- The query is NOT re-executed during validation phase, phantom reads are
    -- not detected. That is, other committed transactions may have added,
    -- updated, or removed keys that impact the result set, and this would not
    -- be detected at validation/commit time.  Applications susceptible to this
    -- should therefore not use GetQueryResult as part of transactions that update
    -- ledger, and should limit use to read-only chaincode operations.
    -- getQueryResult :: ccs -> String -> Either Error StateQueryIterator
    -- GetQueryResultWithPagination performs a "rich" query against a state database.
    -- It is only supported for state databases that support rich query,
    -- e.g., CouchDB. The query string is in the native syntax
    -- of the underlying state database. An iterator is returned
    -- which can be used to iterate over keys in the query result set.
    -- When an empty string is passed as a value to the bookmark argument, the returned
    -- iterator can be used to fetch the first `pageSize` of query results.
    -- When the bookmark is a non-emptry string, the iterator can be used to fetch
    -- the first `pageSize` keys between the bookmark and the last key in the query result.
    -- Note that only the bookmark present in a prior page of query results (ResponseMetadata)
    -- can be used as a value to the bookmark argument. Otherwise, an empty string
    -- must be passed as bookmark.
    -- This call is only supported in a read only transaction.
    -- getQueryResultWithPagination :: ccs -> String -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    -- GetHistoryForKey returns a history of key values across time.
    -- For each historic key update, the historic value and associated
    -- transaction id and timestamp are returned. The timestamp is the
    -- timestamp provided by the client in the proposal header.
    -- GetHistoryForKey requires peer configuration
    -- core.ledger.history.enableHistoryDatabase to be true.
    -- The query is NOT re-executed during validation phase, phantom reads are
    -- not detected. That is, other committed transactions may have updated
    -- the key concurrently, impacting the result set, and this would not be
    -- detected at validation/commit time. Applications susceptible to this
    -- should therefore not use GetHistoryForKey as part of transactions that
    -- update ledger, and should limit use to read-only chaincode operations.
    -- getHistoryForKey :: ccs -> Either Error HistoryQueryIterator
    -- GetPrivateData returns the value of the specified `key` from the specified
    -- `collection`. Note that GetPrivateData doesn't read data from the
    -- private writeset, which has not been committed to the `collection`. In
    -- other words, GetPrivateData doesn't consider data modified by PutPrivateData
    -- that has not been committed.
    -- getPrivateData :: ccs -> String -> String -> Either Error ByteString
    -- GetPrivateDataHash returns the hash of the value of the specified `key` from the specified
    -- `collection`
    -- getPrivateDataHash :: ccs -> String -> String -> Either Error ByteString
    -- PutPrivateData puts the specified `key` and `value` into the transaction's
    -- private writeset. Note that only hash of the private writeset goes into the
    -- transaction proposal response (which is sent to the client who issued the
    -- transaction) and the actual private writeset gets temporarily stored in a
    -- transient store. PutPrivateData doesn't effect the `collection` until the
    -- transaction is validated and successfully committed. Simple keys must not
    -- be an empty string and must not start with a null character (0x00) in order
    -- to avoid range query collisions with composite keys, which internally get
    -- prefixed with 0x00 as composite key namespace. In addition, if using
    -- CouchDB, keys can only contain valid UTF-8 strings and cannot begin with an
    -- an underscore ("_").
    -- putPrivateData :: ccs -> String -> String -> ByteString -> Maybe Error
    -- DelPrivateData records the specified `key` to be deleted in the private writeset
    -- of the transaction. Note that only hash of the private writeset goes into the
    -- transaction proposal response (which is sent to the client who issued the
    -- transaction) and the actual private writeset gets temporarily stored in a
    -- transient store. The `key` and its value will be deleted from the collection
    -- when the transaction is validated and successfully committed.
    -- delPrivateData :: ccs -> String -> String -> Maybe Error
    -- SetPrivateDataValidationParameter sets the key-level endorsement policy
    -- for the private data specified by `key`.
    -- setPrivateDataValidationParameter :: ccs -> String -> String -> ByteString -> Maybe Error
    -- GetPrivateDataValidationParameter retrieves the key-level endorsement
    -- policy for the private data specified by `key`. Note that this introduces
    -- a read dependency on `key` in the transaction's readset.
    -- getPrivateDataValidationParameter :: ccs -> String -> String -> Either Error ByteString
    -- GetPrivateDataByRange returns a range iterator over a set of keys in a
    -- given private collection. The iterator can be used to iterate over all keys
    -- between the startKey (inclusive) and endKey (exclusive).
    -- The keys are returned by the iterator in lexical order. Note
    -- that startKey and endKey can be empty string, which implies unbounded range
    -- query on start or end.
    -- Call Close() on the returned StateQueryIteratorInterface object when done.
    -- The query is re-executed during validation phase to ensure result set
    -- has not changed since transaction endorsement (phantom reads detected).
    -- getPrivateDataByRange :: ccs -> String -> String -> String -> Either Error StateQueryIterator
    -- GetPrivateDataByPartialCompositeKey queries the state in a given private
    -- collection based on a given partial composite key. This function returns
    -- an iterator which can be used to iterate over all composite keys whose prefix
    -- matches the given partial composite key. The `objectType` and attributes are
    -- expected to have only valid utf8 strings and should not contain
    -- U+0000 (nil byte) and U+10FFFF (biggest and unallocated code point).
    -- See related functions SplitCompositeKey and CreateCompositeKey.
    -- Call Close() on the returned StateQueryIteratorInterface object when done.
    -- The query is re-executed during validation phase to ensure result set
    -- has not changed since transaction endorsement (phantom reads detected).
    -- getPrivateDataByPartialCompositeKey :: ccs -> String -> String -> [String] -> Either Error StateQueryIterator
    -- GetPrivateDataQueryResult performs a "rich" query against a given private
    -- collection. It is only supported for state databases that support rich query,
    -- e.g.CouchDB. The query string is in the native syntax
    -- of the underlying state database. An iterator is returned
    -- which can be used to iterate (next) over the query result set.
    -- The query is NOT re-executed during validation phase, phantom reads are
    -- not detected. That is, other committed transactions may have added,
    -- updated, or removed keys that impact the result set, and this would not
    -- be detected at validation/commit time.  Applications susceptible to this
    -- should therefore not use GetPrivateDataQueryResult as part of transactions that update
    -- ledger, and should limit use to read-only chaincode operations.
    -- getPrivateDataQueryResult :: ccs -> String -> String -> Either Error StateQueryIterator
    -- GetCreator returns `SignatureHeader.Creator` (e.g. an identity)
    -- of the `SignedProposal`. This is the identity of the agent (or user)
    -- submitting the transaction.
    getCreator :: ccs -> Maybe ByteString

    -- GetTransient returns the `ChaincodeProposalPayload.Transient` field.
    -- It is a map that contains data (e.g. cryptographic material)
    -- that might be used to implement some form of application-level
    -- confidentiality. The contents of this field, as prescribed by
    -- `ChaincodeProposalPayload`, are supposed to always
    -- be omitted from the transaction and excluded from the ledger.
    getTransient :: ccs -> Maybe MapTextBytes

    -- GetBinding returns the transaction binding, which is used to enforce a
    -- link between application data (like those stored in the transient field
    -- above) to the proposal itself. This is useful to avoid possible replay
    -- attacks.
    getBinding :: ccs -> Maybe MapTextBytes

    -- GetDecorations returns additional data (if applicable) about the proposal
    -- that originated from the peer. This data is set by the decorators of the
    -- peer, which append or mutate the chaincode input passed to the chaincode.
    getDecorations :: ccs -> MapTextBytes

    -- GetSignedProposal returns the SignedProposal object, which contains all
    -- data elements part of a transaction proposal.
    getSignedProposal :: ccs -> Maybe Pb.SignedProposal

    -- GetTxTimestamp returns the timestamp when the transaction was created. This
    -- is taken from the transaction ChannelHeader, therefore it will indicate the
    -- client's timestamp and will have the same value across all endorsers.
    getTxTimestamp :: ccs -> Either Error GooglePb.Timestamp

-- SetEvent allows the chaincode to set an event on the response to the
-- proposal to be included as part of a transaction. The event will be
-- available within the transaction in the committed block regardless of the
-- validity of the transaction.
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
    next :: sqi -> ExceptT Error IO Pb.KV
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
