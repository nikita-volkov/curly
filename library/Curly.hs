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

runOp ::
  -- | Timeout in seconds.
  Int ->
  Op a ->
  IO (Either OpErr a)
runOp timeout (Op runOp) = do
  curl <- Curl.curl_easy_init
  Curl.curl_easy_setopt
    curl
    [ Curl.CURLOPT_FOLLOWLOCATION True,
      Curl.CURLOPT_NOPROGRESS True,
      Curl.CURLOPT_VERBOSE False,
      Curl.CURLOPT_TIMEOUT (fromIntegral timeout)
    ]
  res <- runOp curl
  Curl.curl_easy_cleanup curl
  return res

runOpHappily ::
  -- | Timeout in seconds.
  Int ->
  Op a ->
  IO a
runOpHappily timeout op =
  runOp timeout op >>= either (die . renderErr) return
  where
    renderErr = \case
      CurlOpErr err -> "Curl: " <> show err
      BodyParserOpErr err -> "Body parser: " <> toString err

-- * Op

newtype Op a
  = Op (Curl.CURL -> IO (Either OpErr a))
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Curl.CURL (ExceptT OpErr IO))

data OpErr
  = CurlOpErr Curl.CURLE
  | BodyParserOpErr Text
  deriving (Show)

post ::
  -- | URL.
  String ->
  -- | Request headers.
  [(Text, Text)] ->
  -- | Request body.
  ByteString ->
  -- | Response body parser.
  BodyParser body ->
  Op body
post url headers body (BodyParser setBodyParserUp) =
  Op $ \curl -> runExceptT $ do
    lift $ CurlhsExtras.setByteStringReadFunction curl body
    lift $
      Curl.curl_easy_setopt
        curl
        [ Curl.CURLOPT_URL url,
          Curl.CURLOPT_POST True,
          Curl.CURLOPT_HTTPHEADER (prepareHeaders headers),
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
    consumeVar <- newIORef $ Cereal.runGetPartial get
    earlyResVar <- newIORef Nothing
    Curl.curl_easy_setopt
      curl
      [ Curl.CURLOPT_WRITEFUNCTION . Just $ \input -> do
          consume <- readIORef consumeVar
          case consume input of
            Cereal.Partial consume -> do
              writeIORef consumeVar consume
              return Curl.CURL_WRITEFUNC_OK
            Cereal.Done res remainder -> do
              if ByteString.null remainder
                then do
                  writeIORef earlyResVar $ Just $ Right res
                  return Curl.CURL_WRITEFUNC_OK
                else do
                  writeIORef earlyResVar $ Just $ Left "Not all data consumed"
                  return Curl.CURL_WRITEFUNC_FAIL
            Cereal.Fail err remainder -> do
              writeIORef earlyResVar $ Just $ Left err
              return Curl.CURL_WRITEFUNC_FAIL
      ]
    return $ do
      earlyRes <- readIORef earlyResVar
      case earlyRes of
        Just earlyRes -> return $ earlyRes
        Nothing -> do
          consume <- readIORef consumeVar
          return $ case consume mempty of
            Cereal.Partial consume -> Left "Not enough input"
            Cereal.Done res remainder ->
              if ByteString.null remainder
                then Right res
                else Left "Not all data consumed"
            Cereal.Fail err remainder -> Left err

implicitCerealBodyParser :: Cereal.Serialize a => BodyParser a
implicitCerealBodyParser = explicitCerealBodyParser Cereal.get

-- * Helpers

prepareHeaders = fmap prepareHeader

prepareHeader (name, value) =
  toString name <> ": " <> toString value
