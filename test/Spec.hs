{-# LANGUAGE TemplateHaskell #-}

import Test.Hspec
import Test.QuickCheck

import qualified Test.Parse.Patterns as TPP
import qualified Test.Parse.Combinators as TPC

main :: IO ()
main = hspec $ do
    TPP.test
    TPC.test