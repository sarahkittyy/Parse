-- | Defines parser combinators
module Internal.Combinators
( many
, many1
, between
, nOf
, optional
, sepBy
, sepBy1
) where
    
import Internal.Parser

-- | Matches 0 or more of a given parser
many :: Parser a -> Parser [a]
many = (<|> pure []) . many1

-- | Matches 1 or more of a given parser
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many1 p

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

