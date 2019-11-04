-- | Implements basic parser patterns
module Internal.Patterns
( item
, satisfy
, char
) where
    
import Internal.Parser
import Internal.Combinators

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