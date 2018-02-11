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
    | InternalProc (Args Ident) [Internal]
    | InternalBuiltInProc (Args ()) (Func Internal (Either Error Internal))
    -- | InternalCallProc Internal [Internal]
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
data Args a
    = Arg a (Args a)
    | MultiArg a
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
        binaryArgs = Arg () (Arg () NoArg)


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

-- Create internal representation from expressions
internalRepr :: [Expr] -> Either Error [Internal]
internalRepr [] = pure []
internalRepr (e:exprs) = case e of
    ExprAtom a ->
        (:) <$> pure (InternalAtom a) <*> internalRepr exprs
    ExprParens body ->
        (:) . InternalList <$> internalRepr body <*> internalRepr exprs


evalInternal :: Env -> [Internal] -> Either Error [Internal]
evalInternal _ [] = pure []
evalInternal env (x:xs) = case x of
    InternalAtom (AtomIdent i) ->
        (:) <$> lookupEnv env i <*> evalInternal env xs
    a@(InternalAtom _) ->
        (:) <$> pure a <*> evalInternal env xs
    InternalList body ->
        (:) <$> evalParens env body <*> evalInternal env xs
    _ ->
        Left $ EvalError "Not implemented"


-- Evaluates internal representation for ExprParens case of Expr
-- Calls procedures and special language syntax
evalParens :: Env -> [Internal] -> Either Error Internal
evalParens _ q@(InternalAtom (AtomIdent "quote"):rest) = case rest of
    [_] -> pure $ InternalList q
    _ -> Left $ EvalError "Improper use of quote"
evalParens _ (InternalAtom (AtomIdent "lambda"):args:body) =
    evalLambda args body
evalParens env content =
    evalProcCall env =<< evalInternal env content


evalLambda :: Internal -> [Internal] -> Either Error Internal
evalLambda args body = case args of
    InternalAtom (AtomIdent i) ->
        pure $ InternalProc (MultiArg i) body
    InternalList argExprList ->
        let identList :: Either Error [Ident]
            identList = sequence $ map ident argExprList
            makeArgs :: [Ident] -> Args Ident
            makeArgs [] = NoArg
            makeArgs (a:rest) = Arg a $ makeArgs rest
        in InternalProc <$> (makeArgs <$> identList) <*> pure body
    _ ->
        Left $ EvalError "Improper arguments to lambda"


ident :: Internal -> Either Error Ident
ident (InternalAtom (AtomIdent i)) = Right i
ident _ = Left $ EvalError "Expected identificator"


lookupEnv :: Env -> Ident -> Either Error Internal
lookupEnv env name = case Map.lookup name env of
    Nothing -> Left $ EvalError ("Undefined reference " ++ name)
    Just x -> Right x


evalProcCall :: Env -> [Internal] -> Either Error Internal
evalProcCall env (proc:callArgs) = case proc of
    InternalProc procArgs body ->
        last <$> (join . sequence $ evalInternal <$> (injectArgs env <$> formatArgs procArgs callArgs) <*> pure body)
    InternalBuiltInProc args func ->
        let format (x:xs) (Arg _ rest) = (:) <$> pure x <*> format xs rest
            format [] NoArg = pure []
            format xs (MultiArg _) = pure xs
            format _ _ = Left $ EvalError "Wrong arguments"
        in applyFunc func =<< format callArgs args
    unexpected ->
        Left $ EvalError ("Invalid procedure call: Unexpected " ++ show unexpected)
evalProcCall _ [] =
    Left $ EvalError "Invalid procedure call: Empty application () "


formatArgs :: Args Ident -> [Internal] -> Either Error (Args (Ident, Internal))
formatArgs NoArg [] = pure NoArg
formatArgs (MultiArg name) args = pure $ MultiArg (name, InternalList args)
formatArgs (Arg name rest) (a:args) = Arg (name, a) <$> formatArgs rest args
formatArgs _ _ = Left $ EvalError "Invalid arguments to proc"


injectArgs :: Env -> Args (Ident, Internal) -> Env
injectArgs env_ NoArg = env_
injectArgs env_ (MultiArg (key, val)) = Map.insert key val env_
injectArgs env_ (Arg (key, val) rest) = Map.insert key val $ injectArgs env_ rest


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
