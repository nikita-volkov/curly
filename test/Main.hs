module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

main =
  defaultMain . testGroup "All"
    =<< sequence
      []
