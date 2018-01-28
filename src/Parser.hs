module Parser (Parser, BasicToken, parseTokens) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String


data BasicToken
    = TokenNumber Integer
    | TokenDouble Double
    | TokenIdent  String
    | TokenLeftParen
    | TokenRightParen
    deriving (Show)


parseTokens :: String -> Either (ParseError Char Void) [BasicToken]
parseTokens = parse tokensParser ""


symbol :: Parser Char
symbol = oneOf "!$%&*+-./:<=>?@^_~"

--
-- Token parsers
--

numberParser :: Parser BasicToken
numberParser = (TokenNumber . read) <$> some digitChar

doubleParser :: Parser BasicToken
doubleParser = do
        n1 <- some digitChar
        char '.'
        n2 <- some digitChar
        return $ TokenDouble . read $ n1 ++ "." ++ n2

identParser :: Parser BasicToken
identParser = TokenIdent <$> parser
    where parser = (:) <$> (letterChar <|> symbol)
                       <*> many (letterChar <|> symbol <|> digitChar)

rightParenParser :: Parser BasicToken
rightParenParser = char ')' *> pure TokenRightParen

leftParenParser :: Parser BasicToken
leftParenParser = char '(' *> pure TokenLeftParen

-- terminator for lookAhead termination of elements
terminatorParser :: Parser ()
terminatorParser = (oneOf "() " *> pure ()) <|> (eof *> pure ())

-- works:
-- > parse tokensParser "" "(+ 2 s2 35)"
-- Right [Parens [Ident "+",Number 2,Ident "s2",Number 35]]
tokensParser :: Parser [BasicToken]
tokensParser = space
             *> some ((parensParser <|> elementParser) <* space)
             <* eof
    where
        parensParser = leftParenParser <|> rightParenParser
        elementParser = choice elements
        elements = map (\p -> try $ p <* lookAhead terminatorParser)
            [ numberParser
            , identParser
            , doubleParser
            ]
