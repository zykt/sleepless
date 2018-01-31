{-# LANGUAGE LambdaCase #-}

module Main where


import Control.Monad
import Data.Bifunctor as BF
import Data.Void
import Parser


data Atom
    = AtomInteger Integer
    | AtomDouble  Double
    | AtomIdent   String
    deriving (Show)


data Expr
    = ExprAtom   Atom
    | ExprParens [Expr]
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
    let exprs = createExprTree =<< tokens
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
    a@(ExprAtom _) ->
        (:) <$> pure a <*> eval rest
    ExprParens content ->
        (:) <$> evalProcedure content <*> eval rest


evalProcedure :: [Expr] -> Either Error Expr
evalProcedure [] = Left $ EvalError "Can not evaluate empty proc"
evalProcedure (f:args) = case f of
    ExprAtom (AtomIdent "+") -> specialSum args
    _ -> Left $ EvalError "Not implemented"


specialSum :: [Expr] -> Either Error Expr
specialSum [] = Right $ ExprAtom $ AtomInteger 0
specialSum (a:args) = case a of
    n@(ExprAtom (AtomInteger _)) ->
        (s n) =<< specialSum args
    e ->
        Left $ EvalError ("Unexpected argument to sum" ++ show e)
    where
        s :: Expr -> Expr -> Either Error Expr
        s (ExprAtom (AtomInteger n1)) (ExprAtom (AtomInteger n2)) = Right $ ExprAtom $ AtomInteger (n1 + n2)
        s e1 e2 = Left $ EvalError ("Unexpected argument to sum" ++ show e1 ++ " " ++ show e2)


{-
-- Build recursively expression tree
-- separating regions defined by parenthesis into their own subnodes
createExprTree :: [BasicToken] -> Either Error [Expr]
createExprTree [] = Right []
createExprTree (t:tokens) = case t of
    TokenNumber n ->
        (:) <$> pure (IntegerNumber n) <*> createExprTree tokens
    TokenDouble d ->
        (:) <$> pure (DoubleNumber d) <*> createExprTree tokens
    TokenIdent  i ->
        (:) <$> pure (Ident i) <*> createExprTree tokens
    TokenLeftParen ->
        (:) <$> (Parens <$> createParensOld tokens) <*> createExprTree tokens
    TokenRightParen ->
        Right []

createParensOld :: [BasicToken] -> Either Error [Expr]
createParensOld [] = Left $ SyntaxError "Right parenthesis exprected"
createParensOld ts = createExprTree ts
-}


createExprFromAtomToken :: AtomToken -> Expr
createExprFromAtomToken = \case
    AtomTokenInteger n -> ExprAtom $ AtomInteger n
    AtomTokenDouble d -> ExprAtom $ AtomDouble d
    AtomTokenIdent  i -> ExprAtom $ AtomIdent i

recurse :: ([BasicToken], Expr)
        -> ([BasicToken] -> Either Error ([BasicToken], [Expr]))
        -> Either Error ([BasicToken], [Expr])
recurse (ts, e) f = BF.second (e :) <$> f ts


createExprTree :: [BasicToken] -> Either Error [Expr]
createExprTree tokens = bimap id snd $ createTree tokens


createTree :: [BasicToken] -> Either Error ([BasicToken], [Expr])
createTree [] = pure ([], [])
createTree (t:rest) = case t of
    BasicTokenAtom a ->
        recurse (rest, createExprFromAtomToken a) createTree
    BasicTokenLeftParen ->
        let parens = BF.second ExprParens <$> (createParens rest) :: Either Error ([BasicToken], Expr)
        in join $ recurse <$> parens <*> pure createTree
    BasicTokenRightParen ->
        Left $ SyntaxError "Umatched ')'; Unexpected right parenthesis"


createParens :: [BasicToken] -> Either Error ([BasicToken], [Expr])
createParens [] = Left $ SyntaxError "Unexpected eof"
createParens (t:rest) = case t of
    BasicTokenAtom a ->
        recurse (rest, createExprFromAtomToken a) createParens
    BasicTokenLeftParen ->
        let parens = BF.second ExprParens <$> (createParens rest) :: Either Error ([BasicToken], Expr)
        in join $ recurse <$> parens <*> pure createParens
    BasicTokenRightParen ->
        pure (rest, [])

{-

createTree :: ([BasicToken], [Expr]) -> Either Error ([BasicToken], [Expr])
createTree ([], _) = pure ([], [])
createTree (tokens@(t:rest), exprs) = case t of
    TokenNumber n ->
        tokenToExpr (rest, IntegerNumber n)
    TokenDouble d ->
        tokenToExpr (rest, DoubleNumber d)
    TokenIdent  i ->
        tokenToExpr (rest, Ident i)
    TokenLeftParen ->
        tokenToExpr <$> createParens (rest, [])
    TokenRightParen ->
        Left (SyntaxError "Umatched ')'; Unexpected right parenthesis")
    where
        -- returns without used tokens and new expression added
        tokenToExpr :: ([BasicToken], Expr) -> ([BasicToken], [Expr]) -> ([BasicToken], [Expr])
        tokenToExpr (ts, e) = bimap (\_ -> ts) (\es -> e:es) <$> (createTree (ts, e))
        takeOne
        --parensToExpr :: [Expr] -> ([BasicToken], [Expr]) -> ([BasicToken], [Expr])
        --parensToExpr

createParens ::  ([BasicToken], [Expr]) -> Either Error ([BasicToken], Expr)
createParens ([], _) = Left $ SyntaxError "Unexpected eof"
createParens (tokens@(t:rest), exprs) = case t of
    TokenRightParen ->
        Right (rest, Parens exprs)
    _ ->
        createTree (tokens, exprs)
-}
