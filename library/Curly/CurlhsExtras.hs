module Curly.CurlhsExtras where

import Curly.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import Network.CURL730

-- | Recursively updates the read function until the whole bytestring is consumed.
setByteStringReadFunction :: CURL -> ByteString -> IO ()
setByteStringReadFunction handle input =
  if ByteString.null input
    then
      curl_easy_setopt handle $
        [ CURLOPT_READFUNCTION Nothing
        ]
    else
      curl_easy_setopt handle $
        [ CURLOPT_READFUNCTION . Just $ \requestedAmount ->
            case ByteString.splitAt requestedAmount input of
              (toSend, remainder) -> do
                setByteStringReadFunction handle remainder
                return $ CURL_READFUNC_OK toSend
        ]

setAwatingCerealWriteFunction :: CURL -> Cereal.Get res -> IO (IO (Either String res))
setAwatingCerealWriteFunction handle get = do
  resVar <- newEmptyMVar
  setBasicCerealWriteFunction
    handle
    (putMVar resVar . Left)
    (putMVar resVar . Right)
    (Cereal.runGetPartial get)
  return $ readMVar resVar

setBasicCerealWriteFunction ::
  CURL ->
  (String -> IO ()) ->
  (res -> IO ()) ->
  (ByteString -> Cereal.Result res) ->
  IO ()
setBasicCerealWriteFunction handle fail emit consume =
  curl_easy_setopt handle $
    [ CURLOPT_WRITEFUNCTION . Just $ \input ->
        case consume input of
          Cereal.Partial consume -> do
            setBasicCerealWriteFunction handle fail emit consume
            return CURL_WRITEFUNC_OK
          Cereal.Done res remainder -> do
            curl_easy_setopt handle [CURLOPT_WRITEFUNCTION Nothing]
            if ByteString.null remainder
              then do
                fail "Not all data consumed"
                return CURL_WRITEFUNC_FAIL
              else do
                emit res
                return CURL_WRITEFUNC_OK
          Cereal.Fail err remainder -> do
            curl_easy_setopt handle [CURLOPT_WRITEFUNCTION Nothing]
            fail err
            return CURL_WRITEFUNC_FAIL
    ]
