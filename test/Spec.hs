{-# LANGUAGE TemplateHaskell #-}

import Test.Hspec
import Test.QuickCheck

import qualified Test.Internal.Patterns as TIP
import qualified Test.Internal.Combinators as TIC

main :: IO ()
main = hspec $ do
    TIP.test
    TIC.test