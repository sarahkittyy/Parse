-- | Defines parser combinators
module Parse.Combinators
( many
, many1
, between
, nOf
, optional
, sepBy
, sepBy1
, peek
, chainl
, chainl1
, chainr
, chainr1
) where
    
import Parse.Parser

-- | Matches 0 or more of a given parser
many :: Parser a -> Parser [a]
many = (<|> pure []) . many1

-- | Matches 1 or more of a given parser
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

-- | Matches n of a given parser
nOf :: Int -> Parser a -> Parser [a]
nOf 0 _ = pure []
nOf n p = ((:) <$> p <*> nOf (n - 1) p) <|> failure "Could not match enough of the given parser."

-- | Matches zero or one of a given parser
optional :: Parser a -> Parser [a]
optional = (<|> pure []) . ((:[]) <$>)

-- | Separate 0 or more of a parser by a delimiter parser
sepBy :: Parser b -> Parser a -> Parser [a]
sepBy sep = (<|> pure []) . sepBy1 sep
    
-- | Matches one or more of a parser separated by a delimiter
sepBy1 :: Parser b -> Parser a -> Parser [a]
sepBy1 sep p = do
    first <- p
    rest <- (sep >> sepBy1 sep p) <|> pure [] 
    return (first:rest)

-- | Matches between two parsers
between :: Parser b -> Parser c -> Parser a -> Parser a
between l r p = do
    l
    m <- p
    r
    return m
    
-- | Applies parser p without consuming any input
peek :: Parser a -> Parser a
peek p = Parser $ \input -> case parse p input of
                                Left err -> Left err
                                Right (m, rest) -> Right (m, input)

-- | chainl1, but also takes in an optional return value if no parser matches are found.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op alt = chainl1 p op <|> pure alt
                                
-- | Useful for arithmetic -- kinda like foldr, used for parsing left-associative expressions
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    a <- p
    rest a
    where
        rest a = ( do
            fn <- op
            b <- p
            rest (fn a b) )
                <|> return a
           
-- | Like chainr1 but with a default alternative initial value in the case of no matches
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op alt = chainr1 p op <|> pure alt
                
-- | Parses [p] separated by Parser f that returns a binary function, which is applied as a foldl to all matches of p
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
    a <- p
    rest a
    where
        rest a = ( do
            fn <- op
            b <- do { a <- p; rest a }
            return $ fn a b )
                <|> return a