module Utils.Serializer 
  ( encodeMessage
  , decodeMessage
  ) where

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL

-- Serialize data thành String (để gửi qua network)
encodeMessage :: ToJSON a => a -> String
encodeMessage = BL.unpack . encode

-- Deserialize String thành data
decodeMessage :: FromJSON a => String -> Maybe a
decodeMessage = decode . BL.pack