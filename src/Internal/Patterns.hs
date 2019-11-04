-- | Implements basic parser patterns
module Internal.Patterns
( item
, satisfy
, char
, oneOf
, string
, digit
, natural
, integer
, number
, spacing
) where
    
import Internal.Parser
import Internal.Combinators
import Data.Char
import Control.Monad (when)

-- | Matches a single char
item :: Parser Char
item = Parser $ \input ->
    if null input
        then Left "No characters left in stream."
        else Right (head input, tail input)
        
-- | Parses a char matching the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = item >>= \res -> if pred res
                                    then pure res
                                    else failure $ "Predicate not satisfied on character " ++ [res] ++ "."

-- | Matches a specific char
char :: Char -> Parser Char
char ch = satisfy (==ch)

-- | Matches one of any of the given characters
oneOf :: [Char] -> Parser Char
oneOf chars = satisfy (`elem` chars)

-- | Matches a given string
string :: String -> Parser String
string "" = pure ""
string (c:cs) = char c >> string cs >> return (c:cs)

-- | Matches a digit
digit :: Parser Char
digit = satisfy isDigit

-- | Matches a whole integer
natural :: Parser String
natural = many1 digit

-- | Matches any integer
integer :: Parser String
integer = do
    sign <- optional (char '-')
    num <- natural
    return $ sign ++ num
    
-- | Matches any number
number :: Parser String
number = do
    sign <- optional (char '-')
    whole <- natural <|> pure "" 
    decimal <- ( do
        pt <- char '.' 
        fract <- natural
        return $ pt:fract ) <|> pure "" 
    when (null whole && null decimal) $ failure "No valid number given."
    return $ sign ++ whole ++ decimal
    
-- | Matches any amount of whitespace
spacing :: Parser String
spacing = (many1 $ satisfy isSpace) <|> failure "Whitespace expected"