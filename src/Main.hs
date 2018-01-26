module Main where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char


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

symbol :: Stream s m Char => ParsecT s u m Char
symbol = oneOf "!$%&*+-./:<=>?@^_~"

numberParser :: Stream s m Char => ParsecT s u m Expr
numberParser = (Number . read) <$> many1 digit

identParser :: Stream s m Char => ParsecT s u m Expr
identParser = Ident <$> parser
  where parser = (:) <$> (letter <|> symbol)
                     <*> (many (letter <|> symbol <|> digit))

parensParser :: Stream s m Char => ParsecT s u m Expr
parensParser = do
  char '('
  content <- exprParser
  char ')'
  return $ Parens content

-- works:
-- > parse exprParser "" "(+ 2 s2 35)"
-- Right [Parens [Ident "+",Number 2,Ident "s2",Number 35]]
exprParser :: Stream s m Char => ParsecT s u m [Expr]
exprParser = many1 $ spaces >> (choiceTry parsers)
  where
    choiceTry = choice . (map try)
    parsers =
      [ numberParser
      , identParser
      , parensParser
      ]
