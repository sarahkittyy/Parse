{-# LANGUAGE TemplateHaskell #-}

import Test.Hspec
import Test.QuickCheck

import qualified Test.Parse.Patterns as TPP
import qualified Test.Parse.Combinators as TPC
import qualified Test.Parse.Parser as TPPS

main :: IO ()
main = hspec $ do
    TPP.test
    TPC.test
    TPPS.test