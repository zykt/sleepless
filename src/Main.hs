module Main where

import Data.Void
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Expr
  = Number Integer
  | Ident  String
  | Parens [Expr]
  deriving (Show)


main :: IO ()
main = do
  putStr "> "
  str <- getLine
  let result = show $ parse exprParser "" str
  putStrLn result
  when (str /= "(exit)") main

symbol :: Parser Char
symbol = oneOf "!$%&*+-./:<=>?@^_~"

numberParser :: Parser Expr
numberParser = (Number . read) <$> some digitChar

identParser :: Parser Expr
identParser = Ident <$> parser
  where parser = (:) <$> (letterChar <|> symbol)
                     <*> (many (letterChar <|> symbol <|> digitChar))

parensParser :: Parser Expr
parensParser = do
  char '('
  content <- exprParser
  char ')'
  return $ Parens content

-- works:
-- > parse exprParser "" "(+ 2 s2 35)"
-- Right [Parens [Ident "+",Number 2,Ident "s2",Number 35]]
exprParser :: Parser [Expr]
exprParser = many $ space >> (choiceTry parsers)
  where
    choiceTry = choice . (map try)
    parsers =
      [ numberParser
      , identParser
      , parensParser
      ]
