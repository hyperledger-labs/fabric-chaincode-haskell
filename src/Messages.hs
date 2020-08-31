{-# LANGUAGE OverloadedStrings #-}

module Messages where

import qualified Data.ByteString.UTF8          as BSU
import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString               as BS
import           Data.Text
import           Data.Text.Encoding            as TSE
import           Data.Text.Lazy                 ( fromStrict )

import           Network.GRPC.HighLevel.Generated
import qualified Network.GRPC.LowLevel.Client  as Client
import           Proto3.Suite                  as Suite
import           Proto3.Wire.Encode            as Wire
import           Proto3.Wire.Decode            as Wire
import           Proto3.Wire
import           Peer.ChaincodeShim            as Pb
import           Peer.Chaincode                as Pb
import           Peer.ProposalResponse         as Pb


data CCMessageType = GET_STATE | PUT_STATE | DEL_STATE | REGISTER | COMPLETED | GET_STATE_BY_RANGE | QUERY_STATE_NEXT

regMessage :: ChaincodeMessage
regMessage = buildChaincodeMessage REGISTER regPayload "" ""

regPayload :: Pb.ChaincodeID
regPayload = Pb.ChaincodeID
  { chaincodeIDName    = "mycc:v0" -- Go shim only sends this
  , chaincodeIDPath    = ""
  , chaincodeIDVersion = ""
  }

successPayload :: Maybe ByteString -> Pb.Response
successPayload Nothing = Pb.Response
  { responseStatus  = 200
  , responseMessage = ""
  , responsePayload = TSE.encodeUtf8 ""
  }
successPayload (Just payload) = Pb.Response
  { responseStatus  = 200
  , responseMessage = ""
  , responsePayload = payload
  }

errorPayload :: Text -> Pb.Response
errorPayload message = Pb.Response
  { responseStatus  = 400
  , responseMessage = fromStrict message
  , responsePayload = TSE.encodeUtf8 ""
  }

getStatePayload :: Text -> Pb.GetState
getStatePayload key =
  Pb.GetState {getStateKey = fromStrict key, getStateCollection = ""}

putStatePayload :: Text -> BS.ByteString -> Pb.PutState
putStatePayload key value = Pb.PutState
  { putStateKey        = fromStrict key
  , putStateValue      = value
  , putStateCollection = ""
  }

delStatePayload :: Text -> Pb.DelState
delStatePayload key =
  Pb.DelState {delStateKey = fromStrict key, delStateCollection = ""}

getStateByRangePayload :: Text -> Text -> Pb.GetStateByRange
getStateByRangePayload startKey endKey = Pb.GetStateByRange {
    getStateByRangeStartKey = fromStrict startKey
    , getStateByRangeEndKey = fromStrict endKey
    , getStateByRangeCollection = ""
    , getStateByRangeMetadata = BSU.fromString ""
}

queryNextStatePayload :: Text -> Pb.QueryStateNext
queryNextStatePayload id =
  Pb.QueryStateNext { queryStateNextId = fromStrict id }

-- buildChaincodeMessage
--   :: Enumerated Pb.ChaincodeMessage_Type
--   -> a
--   -> Text
--   -> Text
--   -> ChaincodeMessage
buildChaincodeMessage mesType payload txid chanID = ChaincodeMessage
  { chaincodeMessageType           = getCCMessageType mesType
  , chaincodeMessageTimestamp      = Nothing
  , chaincodeMessagePayload        = LBS.toStrict
                                     $ Wire.toLazyByteString
                                     $ encodeMessage (FieldNumber 1) payload
  , chaincodeMessageTxid           = fromStrict txid
  , chaincodeMessageProposal       = Nothing
  , chaincodeMessageChaincodeEvent = Nothing
  , chaincodeMessageChannelId      = fromStrict chanID
  }

getCCMessageType :: CCMessageType -> Enumerated Pb.ChaincodeMessage_Type
getCCMessageType ccMessageType = case ccMessageType of
  GET_STATE -> Enumerated $ Right ChaincodeMessage_TypeGET_STATE
  PUT_STATE -> Enumerated $ Right ChaincodeMessage_TypePUT_STATE
  DEL_STATE -> Enumerated $ Right ChaincodeMessage_TypeDEL_STATE
  REGISTER  -> Enumerated $ Right ChaincodeMessage_TypeREGISTER
  COMPLETED -> Enumerated $ Right ChaincodeMessage_TypeCOMPLETED
  GET_STATE_BY_RANGE ->
    Enumerated $ Right ChaincodeMessage_TypeGET_STATE_BY_RANGE
  QUERY_STATE_NEXT -> Enumerated $ Right ChaincodeMessage_TypeQUERY_STATE_NEXT
