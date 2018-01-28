module Main where


import Control.Monad
import Parser



data Expr
    = IntegerNumber Integer
    | DoubleNumber Double
    | Ident  String
    | Parens [Expr]
    deriving (Show)


data SyntaxError = SyntaxError String
    deriving (Show)

main :: IO ()
main = do
    putStr "> "
    str <- getLine
    let tokens = parseTokens str
    let result = fmap createTree tokens
    print $ show tokens
    print $ show result
    when (str /= "(exit)") main


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
