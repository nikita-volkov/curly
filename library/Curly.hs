module Curly
  ( -- * Execution
    runOpHappily,

    -- * Op
    Op,
    post,

    -- * BodyParser
    BodyParser,
    explicitCerealBodyParser,
    implicitCerealBodyParser,
  )
where

import qualified Curly.CurlhsExtras as CurlhsExtras
import Curly.Prelude hiding (Handle, Op, Version)
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import qualified Network.CURL730 as Curl

runOp :: Op a -> IO (Either OpErr a)
runOp (Op runOp) = do
  curl <- Curl.curl_easy_init
  Curl.curl_easy_setopt
    curl
    [ Curl.CURLOPT_FOLLOWLOCATION True,
      Curl.CURLOPT_NOPROGRESS True
    ]
  res <- runOp curl
  Curl.curl_easy_cleanup curl
  return res

runOpHappily :: Op a -> IO a
runOpHappily op =
  runOp op >>= either (fail . show) return

-- * Op

newtype Op a
  = Op (Curl.CURL -> IO (Either OpErr a))

data OpErr
  = CurlOpErr Curl.CURLE
  | BodyParserOpErr Text
  deriving (Show)

post ::
  -- | URL.
  String ->
  -- | Request body.
  ByteString ->
  -- | Response body parser.
  BodyParser body ->
  Op body
post url body (BodyParser setBodyParserUp) =
  Op $ \curl -> runExceptT $ do
    lift $ CurlhsExtras.setByteStringReadFunction curl body
    lift $
      Curl.curl_easy_setopt
        curl
        [ Curl.CURLOPT_URL url,
          Curl.CURLOPT_POST True,
          Curl.CURLOPT_POSTFIELDSIZE_LARGE (fromIntegral . ByteString.length $ body)
        ]
    awaitBody <- lift $ setBodyParserUp curl
    ExceptT $ catch (Right <$> Curl.curl_easy_perform curl) (return . Left . CurlOpErr)
    ExceptT $ first (BodyParserOpErr . fromString) <$> awaitBody

-- * BodyParser

newtype BodyParser a
  = BodyParser (Curl.CURL -> IO (IO (Either String a)))

explicitCerealBodyParser :: Cereal.Get a -> BodyParser a
explicitCerealBodyParser get =
  BodyParser $ \curl -> do
    CurlhsExtras.setAwatingCerealWriteFunction curl get

implicitCerealBodyParser :: Cereal.Serialize a => BodyParser a
implicitCerealBodyParser = explicitCerealBodyParser Cereal.get
