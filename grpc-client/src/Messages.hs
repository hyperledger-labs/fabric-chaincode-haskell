{-# LANGUAGE OverloadedStrings #-}

module Messages where

import           Data.ByteString.Lazy          as LBS
import           Data.ByteString               as BS
import           Data.Text.Lazy
import           Data.Text.Encoding            as TSE

import           Network.GRPC.HighLevel.Generated
import qualified Network.GRPC.LowLevel.Client  as Client
import           Proto3.Suite                  as Suite
import           Proto3.Wire.Encode            as Wire
import           Proto3.Wire.Decode            as Wire
import           Proto3.Wire
import           Peer.ChaincodeShim            as Pb
import           Peer.Chaincode                as Pb
import           Peer.ProposalResponse         as Pb


regPayload :: Pb.ChaincodeID
regPayload = Pb.ChaincodeID
  { chaincodeIDName    = "mycc:v0"
  , chaincodeIDPath    = "chaincodedev/chaincode/chaincode_example02/go"
  , chaincodeIDVersion = "v0"
  }

successPayload :: Pb.Response
successPayload = Pb.Response
  { responseStatus  = 200
  , responseMessage = "Successfully initialised"
  , responsePayload = TSE.encodeUtf8 "40"
  }

getStatePayload :: Text -> Pb.GetState
getStatePayload key = Pb.GetState {getStateKey = key, getStateCollection = ""}

putStatePayload :: Text -> BS.ByteString -> Pb.PutState
putStatePayload key value = Pb.PutState
  { putStateKey        = key
  , putStateValue      = value
  , putStateCollection = ""
  }

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
  , chaincodeMessageTxid           = txid
  , chaincodeMessageProposal       = Nothing
  , chaincodeMessageChaincodeEvent = Nothing
  , chaincodeMessageChannelId      = chanID
  }

regMessage :: ChaincodeMessage
regMessage = buildChaincodeMessage REGISTER regPayload "mytxid" "myc"

initCompletedMessage :: Text -> Text -> Pb.Response -> ChaincodeMessage
initCompletedMessage txID chanID res = ChaincodeMessage
  { chaincodeMessageType = Enumerated $ Right ChaincodeMessage_TypeCOMPLETED
  , chaincodeMessageTimestamp      = Nothing
  , chaincodeMessagePayload        = LBS.toStrict
    $ Wire.toLazyByteString
    $ encodeMessage (FieldNumber 2) res
  , chaincodeMessageTxid           = txID
  , chaincodeMessageProposal       = Nothing
  , chaincodeMessageChaincodeEvent = Nothing
  , chaincodeMessageChannelId      = chanID
  }

data CCMessageType = GET_STATE | PUT_STATE | REGISTER

getCCMessageType :: CCMessageType -> Enumerated Pb.ChaincodeMessage_Type
getCCMessageType ccMessageType = case ccMessageType of
  GET_STATE -> Enumerated $ Right ChaincodeMessage_TypeGET_STATE
  PUT_STATE -> Enumerated $ Right ChaincodeMessage_TypePUT_STATE
  REGISTER  -> Enumerated $ Right ChaincodeMessage_TypeREGISTER
