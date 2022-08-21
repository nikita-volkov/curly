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
    [ testProperty "local" $ \(value :: String) -> unsafePerformIO $ do
        EchoHttpServer.testWith $ \port -> do
          response <-
            let url = "http://127.0.0.1:" <> show port
                body = Cereal.encode value
             in Curly.runOpHappily $ Curly.post url [] body Curly.implicitCerealBodyParser
          return $ value === response
    ]
