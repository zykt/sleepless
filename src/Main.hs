module Main where


import Control.Monad
import Parser

data Expr
    = Number Integer
    | Ident  String
    | Parens [Expr]
    deriving (Show)

main :: IO ()
main = do
    putStr "> "
    str <- getLine
    let result = show $ parseTokens str
    putStrLn result
    when (str /= "(exit)") main
