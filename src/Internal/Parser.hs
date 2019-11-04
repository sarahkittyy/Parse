{-# LANGUAGE InstanceSigs #-}

-- | Base parser logic and instancing
module Internal.Parser
( Parser(..)
, Parseable
, runParser
, parseData
, (<|>)
, failure
, mzero
, mplus
) where
    
import Control.Applicative
import Control.Monad
    
-- | The main parser data type.
data Parser a = Parser { parse :: String -> Either String (a, String) }

-- | Runs a parser atop a string
runParser :: Parser a -> String -> Either String a
runParser p input = fst <$> parse p input

-- | Tries to parse the parseable datatype with the given string
parseData :: Parseable a => String -> Either String a
parseData = runParser parser

-- | Applies a function to the result of a parser.
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap fn p = Parser $ \input ->
        case parse p input of
            Left err -> Left err
            Right (m, rest) -> Right (fn m, rest)
    
-- | Applying the function returned from a parser on another parser
instance Applicative Parser where
    pure :: a -> Parser a
    pure n = Parser $ \input -> Right (n, input)
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> p = Parser $ \input ->
        case parse pf input of
            Left err -> Left err
            Right (fn, rest) ->
                case parse p rest of
                    Left err -> Left err
                    Right (m, final) -> Right (fn m, final)
                   
-- | Monadic parser combinatorics
instance Monad Parser where
    return :: a -> Parser a
    return = pure
    
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= fn = Parser $ \input ->
        case parse p input of
            Left err -> Left err
            Right (m, rest) -> parse (fn m) rest
            
-- | For fallback parsing
failure :: String -> Parser a
failure = Parser . const . Left
instance Alternative Parser where
    empty :: Parser a
    empty = failure "Parser error."
    
    (<|>) :: Parser a -> Parser a -> Parser a
    main <|> alt = Parser $ \input ->
        case parse main input of
            Left err -> parse alt input
            res -> res
-- | Equivalent to Alternative
instance MonadPlus Parser where
    mzero :: Parser a
    mzero = empty
    
    mplus :: Parser a -> Parser a -> Parser a
    mplus = (<|>)
    
-- | Typeclass that data structures can inherit to become parseable
class Parseable a where
    -- | Defines a parser for the data type
    parser :: Parser a