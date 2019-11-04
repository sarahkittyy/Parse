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
        describe "string" $ do
            it "matches a specific string of text" $ do
                runParser (string "hello") "hello, world" `shouldBe` (Right "hello")
                runParser (string "hi, there") "hi there" `shouldSatisfy` (isLeft)
                runParser (string "hi, there") "hi, there!" `shouldBe` (Right "hi, there")
        describe "digit" $ do
            it "matches a single digit" $ do
                runParser digit "5" `shouldBe` (Right '5')
                runParser digit "1234" `shouldBe` (Right '1')
                runParser digit "hello" `shouldSatisfy` (isLeft)
        describe "natural" $ do
            it "matches a natural number" $ do
                runParser natural "123" `shouldBe` (Right "123")
                runParser natural "hello" `shouldSatisfy` (isLeft)
                runParser natural "-23" `shouldSatisfy` (isLeft)
                runParser natural "4" `shouldBe` (Right "4")
        describe "integer" $ do
            it "matches a positive / negative integer" $ do
                runParser integer "-134" `shouldBe` (Right "-134")
                runParser integer "215.4" `shouldBe` (Right "215")
                runParser integer "-0" `shouldBe` (Right "-0")
        describe "number" $ do
            it "matches any number" $ do
                runParser number "5" `shouldBe` (Right "5")
                runParser number "-5" `shouldBe` (Right "-5")
                runParser number "23.45" `shouldBe` (Right "23.45")
                runParser number "23." `shouldBe` (Right "23")
                runParser number "-.23" `shouldBe` (Right "-.23")
                runParser number ".24" `shouldBe` (Right ".24")