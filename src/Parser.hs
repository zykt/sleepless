module Parser (Parser, BasicToken(..), AtomToken(..), ParseError(..), parseTokens) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String


data AtomToken
    = AtomTokenInteger Integer
    | AtomTokenDouble Double
    | AtomTokenIdent  String
    deriving (Show)


data BasicToken
    = BasicTokenAtom AtomToken
    | BasicTokenLeftParen
    | BasicTokenRightParen
    | BasicTokenQuote
    deriving (Show)


parseTokens :: String -> Either (ParseError Char Void) [BasicToken]
parseTokens = parse tokensParser ""


symbol :: Parser Char
symbol = oneOf "!$%&*+-./:<=>?@^_~"

--
-- Token parsers
--

numberParser :: Parser BasicToken
numberParser = (BasicTokenAtom . AtomTokenInteger . read) <$> some digitChar

doubleParser :: Parser BasicToken
doubleParser = do
        n1 <- some digitChar
        char '.'
        n2 <- some digitChar
        return $ BasicTokenAtom . AtomTokenDouble . read $ n1 ++ "." ++ n2

identParser :: Parser BasicToken
identParser = BasicTokenAtom . AtomTokenIdent <$> parser
    where parser = (:) <$> (letterChar <|> symbol)
                       <*> many (letterChar <|> symbol <|> digitChar)

rightParenParser :: Parser BasicToken
rightParenParser = char ')' *> pure BasicTokenRightParen

leftParenParser :: Parser BasicToken
leftParenParser = char '(' *> pure BasicTokenLeftParen

quoteParser :: Parser BasicToken
quoteParser = char '\'' *> pure BasicTokenQuote

-- terminator for lookAhead termination of elements
terminatorParser :: Parser ()
terminatorParser = (oneOf "() " *> pure ()) <|> (eof *> pure ())

-- works:
-- > parse tokensParser "" "(+ 2 s2 35)"
-- Right [Parens [Ident "+",Number 2,Ident "s2",Number 35]]
tokensParser :: Parser [BasicToken]
tokensParser = space
             *> some ((parensParser <|> elementParser <|> quoteParser) <* space)
             <* eof
    where
        parensParser = leftParenParser <|> rightParenParser
        elementParser = choice elements
        elements = map (\p -> try $ p <* lookAhead terminatorParser)
            [ numberParser
            , identParser
            , doubleParser
            ]
