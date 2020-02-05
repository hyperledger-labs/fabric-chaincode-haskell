module Stub where

import Prelude
import Data
import qualified Interfaces as If

-- Algebraic Type represeting the DefaultChaincodeStub.  This is the 
-- one used to enable the chaincode to interact with ledger and chaincode
-- execution services of the peer. A istance of this type is created for
-- each of the chaincode invocations that are performed.
data DefaultChaincodeStub = DefaultChaincodeStub { 
    -- chaincode invocation arguments. serialised as arrays of bytes.
    args :: [ByteString],
    -- name of the function being invoked.
    function :: String,
    -- arguments of the function idenfied by the chaincode invocation.
    parameters :: [String],
    -- transaction identifier.
    txId :: String,
    -- timestamp of the transaction invocation
    txTimestamp :: GooglePb.Timestamp,
    -- bytes of the X.509 identity of the originator of the transaction.
    creator :: ByteString,
    -- information about the signed proposalgit 
    signedProposal :: Pb.SignedProposal,
    transient :: MapStringBytes,
    binding :: MapStringBytes,
    decorations :: MapStringBytes
}

-- Default error value used for functions that are not implemented
-- yet. This is used to save creation of Error values that are
-- effectively all the same.
NotImplemented = (Error "Function not implemented")


--
instance If.ChaincodeStub DefaultChaincodeStub where
    -- getArgs :: ccs -> [ByteString]
    getArgs ccs = args

    -- getStringArgs :: ccs -> [String]
    getStringArgs ccs = map (\ ) args
   
    -- getFunctionAndParameters :: ccs -> (String, [String]) 
    getFunctionAndParameters ccs = (function, parameters)

    -- getArgsSlice :: ccs -> Either Error ByteString 
    getArgsSlice ccs  = Left NotImplemented 

    -- getTxId :: ccs -> String
    getTxId ccs = txId

    -- getChannelId :: ccs -> String
    getChannelId ccs = channelId

    -- invokeChaincode :: ccs -> String -> [ByteString] -> String -> Pb.Response 
    invokeChaincode ccs cc params = Pb.Response( responseStatus = 500, responseMessage = message(NotImplemented), responsePayload = Nothing )

    -- getState :: ccs -> String -> Either Error ByteString
    getState ccs key = Left NotImplemented

    -- putState :: ccs -> String -> ByteString -> Maybe Error
    putState ccs key value = Right NotImplemented
    
    -- delState :: ccs -> String -> Maybe Error
    delState ccs key = Right NotImplemented

    -- setStateValidationParameter :: ccs -> String -> [ByteString] -> Maybe Error
    setStateValidationParameter ccs key parameters = Right NotImplemented

    -- getStateValiationParameter :: ccs -> String -> Either Error [ByteString]
    getStateValiationParameter ccs key = Left NotImplemented

    -- getStateByRange :: ccs -> String -> String -> Either Error StateQueryIterator
    getStateByRange ccs startKey endKey = Left NotImplemented

    -- getStateByRangeWithPagination :: ccs -> String -> String -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    getStateByRangeWithPagination ccs startKey endKey pageSize, bookmark = Left NotImplemented

    -- getStateByPartialCompositeKey :: ccs -> String -> [String] -> Either Error StateQueryIterator
    getStateByPartialCompositeKey ccs objectType keys  = Left NotImplemented

    --getStateByPartialCompositeKeyWithPagination :: ccs -> String -> [String] -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    getStateByPartialCompositeKeyWithPagination ccs objectType keys pageSize bookmark = Left NotImplemented

    --createCompositeKey :: ccs -> String -> [String] -> Either Error String
    createCompositeKey ccs objectType keys = Left NotImplemented
    
    --splitCompositeKey :: ccs -> String -> Either Error (String, [String])
    splitCompositeKey ccs key = Left NotImplemented

    --getQueryResult :: ccs -> String -> Either Error StateQueryIterator
    getQueryResult ccs query = Left NotImplemented

    --getQueryResultWithPagination :: ccs -> String -> Int32 -> String -> Either Error (StateQueryIterator, Pb.QueryResponseMetadata)
    getQueryResultWithPagination ccs key pageSize bookmark = Left NotImplemented

    --getHistoryForKey :: ccs -> String -> Either Error HistoryQueryIterator
    getHistoryForKey ccs key = Left NotImplemented

    --getPrivateData :: ccs -> String -> String -> Either Error ByteString
    getPrivateData ccs collection key = Left NotImplemented

    --getPrivateDataHash :: ccs -> String -> String -> Either Error ByteString
    getPrivateDataHash ccs collection key = Left NotImplemented

    --putPrivateData :: ccs -> String -> String -> ByteString -> Maybe Error
    putPrivateData ccs collection string value = Right NotImplemented

    --delPrivateData :: ccs -> String -> String -> Maybe Error
    delPrivateData ccs collection key = Right NotImplemented

    --setPrivateDataValidationParameter :: ccs -> String -> String -> ByteArray -> Maybe Error
    setPrivateDataValidationParameter ccs collection key params = Right NotImplemented
    
    --getPrivateDataValidationParameter :: ccs -> String -> String -> Either Error ByteString
    getPrivateDataValidationParameter ccs collection key = Left NotImplemented

    --getPrivateDataByRange :: ccs -> String -> String -> String -> Either Error StateQueryIterator
    getPrivateDataByRange ccs collection startKey endKey = Left NotImplemented

    --getPrivateDataByPartialCompositeKey :: ccs -> String -> String -> [String] -> Either Error StateQueryIterator
    getPrivateDataByPartialCompositeKey ccs collection objectType keys = Left NotImplemented

    -- getPrivateDataQueryResult :: ccs -> String -> String -> Either Error StateQueryIterator
    getPrivateDataQueryResult ccs collection query  = Left NotImplemented

    -- getCreator :: ccs -> Either Error ByteArray
    getCreator ccs = Right creator 

    -- getTransient :: ccs -> Either Error MapStringBytes
    getTransient ccs = Right transient 

    -- getBinding :: ccs -> Either Error MapStringBytes
    getBinding ccs = Right binding 

    -- getDecorations :: ccs -> MapStringBytes
    getDecorations ccs = Right decorations

    -- getSignedProposal :: ccs -> Either Error Pb.SignedProposal
    getSignedProposal ccs = Right signedProposal

    -- getTxTimestamp :: ccs -> Either Error GooglePb.Timestamp
    getTxTimestamp ccs = Right txTimestamp

    -- setEvent :: ccs -> String -> ByteArray -> Maybe Error
    setEvent ccs = Right NotImplemented