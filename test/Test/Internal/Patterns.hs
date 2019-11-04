module Test.Internal.Patterns where
    
import Test.Hspec
import Test.QuickCheck

import Internal.Patterns
import Internal.Parser

import Data.Either
import Data.Char
    
test :: SpecWith ()
test = describe "Patterns" $ do
        describe "item" $ do
            it "retrieves a single character" $ do
                runParser item "hello" `shouldBe` (Right 'h')
                runParser item "owo" `shouldBe` (Right 'o')
        describe "satisfy" $ do
            it "retrieves a character satisfying a condition" $ do
                runParser (satisfy (=='c')) "hello" `shouldSatisfy` isLeft
                runParser (satisfy (=='c')) "characters" `shouldBe` (Right 'c')
                runParser (satisfy isDigit) "1testing" `shouldBe` (Right '1')
        describe "char" $ do
            it "retrieves a specific character" $ do
                runParser (char 'f') "fellow" `shouldBe` (Right 'f')
                runParser (char 'g') "hello" `shouldSatisfy` isLeft
                runParser (char '-') "-123" `shouldBe` (Right '-')
        describe "oneOf" $ do
            it "retrieves a character from a list of possible candidates" $ do
                runParser (oneOf "abc") "alphabet" `shouldBe` (Right 'a')
                runParser (oneOf "efg") "dog" `shouldSatisfy` (isLeft)