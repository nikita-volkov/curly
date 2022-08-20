module Main where

import qualified EchoHttpServer
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

main =
  defaultMain . testGroup "All" $
    [ testCase "" . EchoHttpServer.testWith $ \port ->
        error "TODO"
    ]
