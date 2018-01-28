module Main where


import Control.Monad
import Data.Bifunctor as BF
import Data.Void
import Parser


data Expr
    = IntegerNumber Integer
    | DoubleNumber Double
    | Ident  String
    | Parens [Expr]
    deriving (Show)


data SyntaxError
    = SyntaxError String
    | TokenParseError (ParseError Char Void)
    deriving (Show)


main :: IO ()
main = do
    putStr "> "
    str <- getLine
    let tokens = BF.first adapter $ parseTokens str
    let result = createTree =<< tokens
    print $ show tokens
    print $ show result
    when (str /= "(exit)") main
    where
        adapter :: ParseError Char Void -> SyntaxError
        adapter e = TokenParseError e


-- Build recursively expression tree
-- separating regions defined by parenthesis into their own subnodes
createTree :: [BasicToken] -> Either SyntaxError [Expr]
createTree [] = Right []
createTree (t:tokens) = case t of
    TokenNumber n ->
        (:) <$> pure (IntegerNumber n) <*> createTree tokens
    TokenDouble d ->
        (:) <$> pure (DoubleNumber d) <*> createTree tokens
    TokenIdent  i ->
        (:) <$> pure (Ident i) <*> createTree tokens
    TokenLeftParen ->
        (:) <$> (Parens <$> createParens tokens) <*> createTree tokens
    TokenRightParen ->
        Right []

createParens :: [BasicToken] -> Either SyntaxError [Expr]
createParens [] = Left $ SyntaxError "Right parenthesis exprected"
createParens ts = createTree ts
