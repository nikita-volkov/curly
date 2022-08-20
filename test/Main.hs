module Main where

import qualified Curly
import qualified Data.Serialize as Cereal
import qualified EchoHttpServer
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

main =
  defaultMain . testGroup "All" $
    [ testProperty "" $ \(value :: String) -> unsafePerformIO $ do
        traceM "Starting server"
        EchoHttpServer.testWith $ \port -> do
          traceM "Running server"
          response <-
            let url = "http://localhost:" <> show port
                body = Cereal.encode value
             in Curly.runOpHappily $ Curly.post url body Curly.implicitCerealBodyParser
          return $ value === response
    ]
