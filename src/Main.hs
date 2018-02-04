{-# LANGUAGE LambdaCase #-}

module Main where


import Control.Monad
import Data.Bifunctor as BF
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void
import Parser


-- Basic element of Expr
data Atom
    = AtomInteger Integer
    | AtomDouble  Double
    | AtomIdent   Ident
    deriving (Show)


-- Representation of program in terms of expressions
data Expr
    = ExprAtom   Atom
    | ExprParens [Expr]
    deriving (Show)


-- Internal representation of program
data Internal
    = InternalAtom Atom
    | InternalList [Internal]
    | InternalProc Args Expr
    | InternalBuiltInProc Args (Func Internal (Either Error Internal))
    | InternalCallProc Ident [Internal]
    deriving (Show)


simpleShowInternal :: Internal -> String
simpleShowInternal i = case i of
    InternalAtom a -> simpleShowAtom a
    InternalList l -> "(" ++ unwords (map simpleShowInternal l) ++ ")"
    _ -> "Error! Show Not implemented!"
    where
        simpleShowAtom (AtomInteger x) = show x
        simpleShowAtom (AtomDouble x) = show x
        simpleShowAtom (AtomIdent x) = x


-- Recursive structure for multiple arguments of procedures
-- always ends in either Multiarg or NoArg
data Args
    = Arg Args
    | MultiArg
    | NoArg
    deriving (Show)


data Func a b
    = Func1 (a -> b)
    | Func2 (a -> a -> b)

instance Show (Func a b) where
    show (Func1 _) = "Func1"
    show (Func2 _) = "Func2"


data Error
    = SyntaxError String
    | TokenParseError (ParseError Char Void)
    | EvalError String
    deriving (Show)


type Ident = String

type Env = Map Ident Internal


defaultEnv :: Env
defaultEnv = Map.singleton "add" (builtInBinaryProc builtInSum)
    where
        builtInBinaryProc proc = InternalBuiltInProc binaryArgs (Func2 proc)
        binaryArgs = Arg (Arg NoArg)


builtInSum :: Internal -> Internal -> Either Error Internal
builtInSum (InternalAtom (AtomInteger i1)) (InternalAtom (AtomInteger i2)) =
    pure $ InternalAtom (AtomInteger (i1 + i2))
builtInSum _ _ = Left $ EvalError "Unexpected argument to sum"


data Config = Config { debug :: Bool }


main :: IO ()
main = do
    let config = Config True
    repl config


repl :: Config -> IO ()
repl config = do
    putStr "> "
    str <- getLine
    let tokens = BF.first TokenParseError $ parseTokens str
    let exprs = makeExprs =<< tokens
    let internal = internalRepr =<< exprs
    let internalEval = evalInternal defaultEnv =<< internal
    when (debug config) $ do
        print $ show tokens
        print $ show exprs
        print $ show internal
        print $ show internalEval
    case internalEval of
        Right xs -> forM_ xs (putStrLn . simpleShowInternal)
        Left err -> print . show $ err
    when (str /= "(exit)") $ repl config

--
-- Evaluation
--

internalRepr :: [Expr] -> Either Error [Internal]
internalRepr [] = pure []
internalRepr (e:exprs) = case e of
    ExprAtom a ->
        (:) <$> pure (InternalAtom a) <*> internalRepr exprs
    ExprParens [] ->
        Left $ EvalError "AQOEWJTGIPOWAENTGOIPNPIO"
    ExprParens body ->
        (:) <$> parens body <*> internalRepr exprs


parens :: [Expr] -> Either Error Internal
parens [] =
    Left $ EvalError "Missing procedure in parenthesis"
parens [ExprAtom (AtomIdent "quote"), body] =
    let quote = \case
            ExprAtom a -> InternalAtom a
            ExprParens list -> InternalList $ map quote list
    in pure $ quote body
parens (ExprAtom (AtomIdent "quote"):_) =
    Left $ EvalError "Improper use of quote"
parens (nameExpr:args) =
    let name = case nameExpr of
            (ExprAtom (AtomIdent i)) -> Right i
            _ -> Left $ EvalError "Expected identificator"
    in InternalCallProc <$> name <*> internalRepr args


evalInternal :: Env -> [Internal] -> Either Error [Internal]
evalInternal _ [] = pure []
evalInternal env (x:xs) = case x of
    a@(InternalAtom _) ->
        (:) <$> pure a <*> evalInternal env xs
    list@(InternalList _) ->
        (:) <$> pure list <*> evalInternal env xs
    InternalCallProc name args ->
        (:) <$> join (callProc env name <$> (evalInternal env args)) <*> evalInternal env xs
    _ -> Left $ EvalError "Not implemented"


lookupEnv :: Env -> Ident -> Either Error Internal
lookupEnv env name = case Map.lookup name env of
    Nothing -> Left $ EvalError ("Undefined reference " ++ name)
    Just x -> Right x


callProc :: Env -> Ident -> [Internal] -> Either Error Internal
callProc env name args = helper =<< (lookupEnv env name)
    where
        helper :: Internal -> Either Error Internal
        helper = \case
            InternalBuiltInProc procArgs f ->
                applyFunc f =<< format args procArgs
            _ -> Left $ EvalError "Not implemented"
        format (x:xs) (Arg rest) = (:) <$> pure x <*> format xs rest
        format [] NoArg = pure []
        format xs MultiArg = pure xs
        format _ _ = Left $ EvalError "Wrong arguments"


applyFunc :: Func Internal (Either Error Internal) -> [Internal] -> Either Error Internal
applyFunc (Func1 f) (x1:_) = f x1
applyFunc (Func2 f) (x1:x2:_) = f x1 x2
applyFunc _ _ = Left $ EvalError "Built-in Function application error"

--
-- Recursive reading of tokens
--

createExprFromAtomToken :: AtomToken -> Expr
createExprFromAtomToken = \case
    AtomTokenInteger n -> ExprAtom $ AtomInteger n
    AtomTokenDouble d -> ExprAtom $ AtomDouble d
    AtomTokenIdent  i -> ExprAtom $ AtomIdent i


makeExprs :: [BasicToken] -> Either Error [Expr]
makeExprs tokens = snd <$> exprList tokens


exprList :: [BasicToken] -> Either Error ([BasicToken], [Expr])
exprList [] = pure ([], [])
exprList (BasicTokenRightParen:_) = Left $ SyntaxError "Umatched ')'; Unexpected right parenthesis"
exprList tokens = do
    (tokens', expr_) <- expr tokens
    (tokens'', exprList_) <- exprList tokens'
    return (tokens'', expr_:exprList_)


expr :: [BasicToken] -> Either Error ([BasicToken], Expr)
expr [] = Left $ SyntaxError "Unexpected eof; Expected expression"
expr (t:rest) = case t of
    BasicTokenAtom a ->
        pure (rest, createExprFromAtomToken a)
    BasicTokenLeftParen ->
        let exprList' [] = Left $ SyntaxError "Unexpected eof"
            exprList' (BasicTokenRightParen:rest_) = pure (rest_, [])
            exprList' tokens = do
                (tokens', expr_) <- expr tokens
                (tokens'', exprList_) <- exprList' tokens'
                return (tokens'', expr_:exprList_)
        in BF.second ExprParens <$> exprList' rest
    BasicTokenRightParen ->
        Left $ SyntaxError "Umatched ')'; Unexpected right parenthesis"
    BasicTokenQuote ->
        let quote expr_ = ExprParens [ExprAtom $ AtomIdent "quote", expr_]
        in BF.second quote <$> expr rest
