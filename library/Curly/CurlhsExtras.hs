module Curly.CurlhsExtras where

import Curly.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import Network.CURL730

setByteStringReadFunction :: CURL -> ByteString -> IO ()
setByteStringReadFunction handle input = do
  var <- newIORef input
  curl_easy_setopt handle $
    [ CURLOPT_READFUNCTION . Just $ \requestedAmount ->
        atomicModifyIORef' var $
          fmap CURL_READFUNC_OK . swap . ByteString.splitAt requestedAmount
    ]
