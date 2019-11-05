# Parse

A parsing library.

## Example

```haskell

import Parse

data Expression = Number Int

instance Parseable Expression where
    parser = parseAddition <|> parseNumber
        where
            parseNumber = Number <$> (read <$> number)
            parseAddition =
                Number <$>
                    (chainl (read <$> number) addop 0)
                where
                    addop = char '+' >> return (+)
                    
-----------

parseData "2+3+4+5" :: Either String Expression -- Right (Number 14)

```