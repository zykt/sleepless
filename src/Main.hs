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


data Error
    = SyntaxError String
    | TokenParseError (ParseError Char Void)
    | EvalError String
    deriving (Show)


main :: IO ()
main = do
    putStr "> "
    str <- getLine
    let tokens = BF.first adapter $ parseTokens str
    let exprs = createTree =<< tokens
    let result = eval =<< exprs
    print $ show tokens
    print $ show exprs
    print "Result:"
    print $ show result
    when (str /= "(exit)") main
    where
        adapter :: ParseError Char Void -> Error
        adapter e = TokenParseError e


eval :: [Expr] -> Either Error [Expr]
eval [] = Right []
eval (expr:rest) = case expr of
    IntegerNumber i ->
        (:) <$> pure (IntegerNumber i) <*> eval rest
    DoubleNumber d ->
        (:) <$> pure (DoubleNumber d) <*> eval rest
    Ident i ->
        (:) <$> pure (Ident i) <*> eval rest
    Parens content ->
        (:) <$> evalProcedure content <*> eval rest

evalProcedure :: [Expr] -> Either Error Expr
evalProcedure [] = Left $ EvalError "Can not evaluate empty proc"
evalProcedure (f:args) = case f of
    Ident "+" -> specialSum args
    _ -> Left $ EvalError "Not implemented"


specialSum :: [Expr] -> Either Error Expr
specialSum [] = Right $ IntegerNumber 0
specialSum (a:args) = case a of
    n@(IntegerNumber _) ->
        (s n) =<< specialSum args
    e ->
        Left $ EvalError ("Unexpected argument to sum" ++ show e)
    where
        s :: Expr -> Expr -> Either Error Expr
        s (IntegerNumber n1) (IntegerNumber n2) = Right $ IntegerNumber (n1 + n2)
        s e1 e2 = Left $ EvalError ("Unexpected argument to sum" ++ show e1 ++ " " ++ show e2)


-- Build recursively expression tree
-- separating regions defined by parenthesis into their own subnodes
createTree :: [BasicToken] -> Either Error [Expr]
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

createParens :: [BasicToken] -> Either Error [Expr]
createParens [] = Left $ SyntaxError "Right parenthesis exprected"
createParens ts = createTree ts
