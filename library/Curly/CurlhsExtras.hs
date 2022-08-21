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
setBasicCerealWriteFunction handle reportError emit consume = do
  var <- newIORef consume
  curl_easy_setopt handle $
    [ CURLOPT_WRITEFUNCTION . Just $ \input -> do
        consume <- readIORef var
        case consume input of
          Cereal.Partial consume -> do
            writeIORef var consume
            return CURL_WRITEFUNC_OK
          Cereal.Done res remainder -> do
            if ByteString.null remainder
              then do
                emit res
                return CURL_WRITEFUNC_OK
              else do
                reportError "Not all data consumed"
                return CURL_WRITEFUNC_FAIL
          Cereal.Fail err remainder -> do
            reportError err
            return CURL_WRITEFUNC_FAIL
    ]
