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
            it "maps a function over the result of a parser" $ do
                runParser ((:" is a char") <$> item) "hello"
                    `shouldBe` (Right "h is a char")
                runParser ((++", world!") <$> string "hello") "hello there!"
                    `shouldBe` (Right "hello, world!")
        describe "instance Applicative" $ do
            it "maps the function result of a parser over the result of a parser" $ do
                let parseAdd = string "add two:" >> return (+2)
                let parseInt = read <$> (:"") <$> digit
                runParser (parseAdd <*> parseInt) "add two:5"
                    `shouldBe` (Right 7)
                runParser (parseAdd <*> parseInt) "add two:7"
                    `shouldBe` (Right 9)
        describe "instance Monad" $ do
            it "binds the result of a parser to the next parser" $ do
                runParser (item >>= \ch -> pure ch) "a"
                    `shouldBe` (Right 'a')
                runParser (string "hello" >>= \h -> string " world") "hello world"
                    `shouldBe` (Right " world")
                runParser (string "hello" >>= \h -> string " world" >> return h) "hello world"
                    `shouldBe` (Right "hello")
                runParser (string "hello" >>= \h -> string " world" >> return h) "hewwo world"
                    `shouldSatisfy` isLeft
        describe "instance Alternative" $ do
            it "provides an alternative parse test if the first fails." $ do
                runParser (char 'c' <|> char 'h') "char"
                    `shouldBe` (Right 'c')
                runParser (char 'c' <|> char 'v') "verbose"
                    `shouldBe` (Right 'v')
                runParser (string "hello" <|> string "hewwo") "hello"
                    `shouldBe` (Right "hello")
                runParser (char 'c' <|> failure "No C") "hello"
                    `shouldBe` (Left "No C")
        describe "instance MonadPlus" $ do
            it "is the monadic version of Alternative" $ do
                runParser (char 'c' `mplus` char 'h') "char"
                    `shouldBe` (Right 'c')
                runParser (char 'c' `mplus` char 'v') "verbose"
                    `shouldBe` (Right 'v')
                runParser (string "hello" `mplus` string "hewwo") "hello"
                    `shouldBe` (Right "hello")
                runParser (char 'c' `mplus` failure "No C") "hello"
                    `shouldBe` (Left "No C")
        describe "instance Semigroup" $ do
            it "combines the semigroup-able return types of two parsers" $ do
                runParser (string "a" <> string "b") "abcde"
                    `shouldBe` (Right "ab")
                runParser (string "f" <> ((:[]) <$> item)) "frabc"
                    `shouldBe` (Right "fr")