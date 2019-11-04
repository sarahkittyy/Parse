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
        describe "nOf" $ do
            it "matches exactly n of a given parser" $ do
                runParser (nOf 0 item) "" `shouldBe` (Right "")
                runParser (nOf 5 item) "hello, world" `shouldBe` (Right "hello")
                runParser (nOf 8 (char 'c')) "ccccccc" `shouldSatisfy` (isLeft)
                runParser (nOf 7 (char 'c')) "ccccccc" `shouldBe` (Right "ccccccc")
        describe "optional" $ do
            it "matches zero or one of a given parser" $ do
                runParser (optional item) "" `shouldBe` (Right "")
                runParser (optional (char 'c')) "char" `shouldBe` (Right "c")
                runParser (optional (char 'c')) "hello" `shouldBe` (Right "")
        describe "sepBy" $ do
            it "matches 0 or more of a parser separated by a delimeter" $ do
                runParser (sepBy spacing item) "a b cdef" `shouldBe` (Right "abc")
                runParser (sepBy spacing (char 'c')) "c c c ccc" `shouldBe` (Right "cccc")
                runParser (sepBy spacing item) "" `shouldBe` (Right "")
        describe "sepBy1" $ do
            it "matches 1 or more of a parser separated by a delimiter" $ do
                runParser (sepBy1 spacing item) "a b cdef" `shouldBe` (Right "abc")
                runParser (sepBy1 spacing (char 'c')) "c c c ccc" `shouldBe` (Right "cccc")
                runParser (sepBy1 spacing item) "" `shouldSatisfy` isLeft
        describe "between" $ do
            it "matches a parser between two parsers" $ do
                runParser (between (char '(') (char ')') item) "(f)" `shouldBe` (Right 'f')
                runParser (between item item item) "abc" `shouldBe` (Right 'b')
                runParser (between (char '(') (char ')') (char 'c')) "(c" `shouldSatisfy` isLeft 
        describe "peek" $ do
            it "checks the parser without consuming input" $ do
                runParser (peek (char 'c') >> item) "char" `shouldBe` (Right 'c')
                runParser (peek (string "lmao") >> item) "lmao wtf" `shouldBe` (Right 'l')
                runParser (peek (string "abc") >> char 'd') "dog" `shouldSatisfy` isLeft