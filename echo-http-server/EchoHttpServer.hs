module EchoHttpServer
  ( start,
  )
where

import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Prelude hiding (app)

-- | Start the echo server blocking until it is ready to process the requests.
start :: Int -> IO ()
start port = do
  gate <- newEmptyMVar
  let onStart =
        putMVar gate ()
  let settings =
        Warp.defaultSettings
          & Warp.setBeforeMainLoop onStart
          & Warp.setPort port
  forkIO $ Warp.runSettings settings app
  readMVar gate

app :: Wai.Application
app request respond = do
  requestBody <- Wai.strictRequestBody request
  respond $ response requestBody
  where
    requestContentType = lookup "content-type" (Wai.requestHeaders request)
    response body = do
      Wai.responseLBS HttpTypes.ok200 headers body
      where
        headers =
          maybe [] (pure . ("content-type",)) requestContentType
