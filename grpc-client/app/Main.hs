{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Lib
import Peer.ChaincodeShim as Shim
import Network.GRPC.HighLevel.Generated

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "localhost"
                            , clientServerPort = 7052
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            , clientAuthority = Nothing
                            }

regMessage :: ChaincodeMessage
regMessage = ChaincodeMessage{
    chaincodeMessageType = ChaincodeMessage_TypeREGISTER,
    chaincodeMessageTimestamp = Nothing
}

main :: IO ()
main = withGRPCClient clientConfig $ \client -> do
    -- contains chaincodeSupportRegister function
    Shim.ChaincodeSupport{..} <- chaincodeSupportClient client
    -- chaincodeSupportRegister :: request BiDiStreaming ChaincodeMessage ChaincodeMessage ->
    --                                 IO (response BiDiStreaming ChaincodeMessage)

    -- ClientBiDiResponse   :: MetadataMap -> StatusCode -> StatusDetails -> ClientResult 'BiDiStreaming response
    -- ClientWriterResponse :: Maybe response -> MetadataMap -> MetadataMap -> StatusCode -> StatusDetails -> ClientResult 'ClientStreaming response

    -- ClientBiDiRequest :: TimeoutSeconds -> MetadataMap -> (LL.ClientCall -> MetadataMap -> StreamRecv response -> StreamSend request -> WritesDone -> IO ()) -> ClientRequest 'BiDiStreaming request response
    -- ClientWriterRequest :: TimeoutSeconds -> MetadataMap -> (StreamSend request -> IO ()) -> ClientRequest 'ClientStreaming request response

    ClientBiDiResponse _meta _status _details result
        <- chaincodeSupportRegister $ ClientBiDiRequest 1 [] (\call mmap recv send _ -> do
            send _

)
    case result of
        Just chaincodeMessage -> print "yay"
        Nothing -> "Client stream failed"
    print "testing"

-- main :: IO ()
-- main = someFunc
