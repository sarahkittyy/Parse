module Test.Parse.Parser where
    
import Test.Hspec
import Test.QuickCheck
    
import Parse.Parser
import Parse.Patterns
import Parse.Combinators

import Data.Either

test :: Spec
test = describe "Parser" $ do
        describe "instance Functor" $ do
            it "Maps a function over the result of a parser" $ do
                runParser ((:" is a char") <$> item) "hello"
                    `shouldBe` (Right "h is a char")
                runParser ((++", world!") <$> string "hello") "hello there!"
                    `shouldBe` (Right "hello, world!")
        describe "instance Applicative" $ do
            it "Maps the function result of a parser over the result of a parser" $ do
                let parseAdd = string "add two:" >> return (+2)
                let parseInt = read <$> (:"") <$> digit
                runParser (parseAdd <*> parseInt) "add two:5"
                    `shouldBe` (Right 7)
                runParser (parseAdd <*> parseInt) "add two:7"
                    `shouldBe` (Right 9)
        describe "instance Monad" $ do
            it "Binds the result of a parser to the next parser" $ do
                runParser (item >>= \ch -> pure ch) "a"
                    `shouldBe` (Right 'a')
                runParser (string "hello" >>= \h -> string " world") "hello world"
                    `shouldBe` (Right " world")
                runParser (string "hello" >>= \h -> string " world" >> return h) "hello world"
                    `shouldBe` (Right "hello")
                runParser (string "hello" >>= \h -> string " world" >> return h) "hewwo world"
                    `shouldSatisfy` isLeft