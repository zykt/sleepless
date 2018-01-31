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

--
-- Evaluation
--

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

--
-- Recursive reading of tokens
--

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


createExprFromAtomToken :: AtomToken -> Expr
createExprFromAtomToken = \case
    AtomTokenInteger n -> ExprAtom $ AtomInteger n
    AtomTokenDouble d -> ExprAtom $ AtomDouble d
    AtomTokenIdent  i -> ExprAtom $ AtomIdent i
