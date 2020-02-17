module Error where

import           Network.GRPC.HighLevel.Generated

data Error = GRPCError GRPCIOError
    | Error String
   deriving (Eq, Show)
