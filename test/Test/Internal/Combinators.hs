module Test.Internal.Combinators where
    
import Test.Hspec
import Test.QuickCheck

import Internal.Patterns
import Internal.Combinators
import Internal.Parser

import Data.Either
    
test :: SpecWith ()
test = describe "Combinators" $ do
        describe "many" $ do
            it "matches 0 or more of a given parser" $ do
                runParser (many item) "hello there" `shouldBe` (Right "hello there")
                runParser (many (char 'c')) "cccchar" `shouldBe` (Right "cccc")
                runParser (many (char 'q')) "hello, world!" `shouldBe` (Right "")
        describe "many1" $ do
            it "matches 1 or more of a given parser" $ do
                runParser (many1 item) "" `shouldSatisfy` (isLeft)
                runParser (many1 item) "hello" `shouldBe` (Right "hello")
                runParser (many1 (char 'c')) "cchello" `shouldBe` (Right "cc")