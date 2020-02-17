module Error where

import           Network.GRPC.HighLevel.Generated

data Error = GRPCError GRPCIOError
    | InvalidArgs
    | Error String
   deriving (Eq, Show)
