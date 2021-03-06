{-# LANGUAGE LambdaCase #-}

module Evaluator where


import Data.Bifunctor as BF
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void
import Parser
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State


-- Basic element of Expr
data Atom
    = AtomInteger Integer
    | AtomDouble  Double
    | AtomIdent   Ident
    | AtomSpecial Ident
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
    | InternalClosure Env Internal
    deriving (Show)


simpleShowInternal :: Internal -> String
simpleShowInternal i = case i of
    InternalAtom a -> simpleShowAtom a
    InternalList [InternalAtom (AtomIdent "quote"), rest] -> '\'' : simpleShowInternal rest
    InternalList l -> "(" ++ unwords (map simpleShowInternal l) ++ ")"
    InternalProc _ _ -> "#proc"
    InternalBuiltInProc _ _ -> "#builtinproc"
    InternalClosure _ x -> "Closure + " ++ simpleShowInternal x
    -- _ -> "Error! Show Not implemented!"
    where
        simpleShowAtom (AtomInteger x) = show x
        simpleShowAtom (AtomDouble x) = show x
        simpleShowAtom (AtomIdent x) = x
        simpleShowAtom (AtomSpecial x) = '#':x


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


type EnvironmentT = State Env

type ExceptEnvironmentT = ExceptT Error EnvironmentT

-- Monad stack for evaluation
type EvaluatorT a b = ExceptEnvironmentT a -> ExceptEnvironmentT b


eval :: Env -> String -> Either Error ([Internal], Env)
eval env input = do
    tokens <- BF.first TokenParseError $ parseTokens input
    exprs <- makeExprs tokens
    internal <- internalRepr exprs
    let (internalEval, newState) = runState (runExceptT $ evalInternal (pure internal)) env
    result <- internalEval
    return (result, newState)


debugEval :: String -> IO ()
debugEval str = do
    let tokens = BF.first TokenParseError $ parseTokens str
    let exprs = makeExprs =<< tokens
    let internal = internalRepr =<< exprs
    --let internalEval = evalInternal $ either throwE pure internal
    print $ show tokens
    print $ show exprs
    print $ show internal
    --print $ show internalEval
    {-let helper :: ExceptEnvironmentT [Internal] -> IO ()
        helper = runExceptT >>> flip evalState defaultEnv >>> \case
            Right xs -> forM_ xs (putStrLn . simpleShowInternal)
            Left err -> print . show $ err
    helper internalEval
    -}


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


evalInternal :: EvaluatorT [Internal] [Internal]
evalInternal body = body >>= \x -> case x of
    [] ->
        pure []
    (InternalAtom (AtomIdent i) : rest) ->
        (:) <$> lookupEnv (pure i) <*> evalInternal (pure rest)
    (a@(InternalAtom _) : rest) ->
        (:) <$> pure a <*> evalInternal (pure rest)
    (InternalList body : rest) ->
        (:) <$> evalParens (pure body) <*> evalInternal (pure rest)
    (InternalClosure closure body : rest) -> do
        -- TODO: separate!!
        parentEnv <- lift get
        lift $ put closure
        result <- evalOneInternal (pure body)
        lift $ put parentEnv
        (:) result <$> evalInternal (pure rest)
    -- unexpected ->
    --     throwE $ EvalError $ "Not implemented " ++ show unexpected ++ " from " ++ show x


evalOneInternal :: EvaluatorT Internal Internal
evalOneInternal internal = do
    internal' <- internal
    head <$> evalInternal (pure [internal'])


-- Evaluates internal representation for ExprParens case of Expr
-- Calls procedures and special language syntax
evalParens :: EvaluatorT [Internal] Internal
evalParens body = body >>= \case
    q@(InternalAtom (AtomIdent "quote") : rest) -> case rest of
        [_] -> pure $ InternalList q
        _ -> throwE $ EvalError "Improper use of quote"
    (InternalAtom (AtomIdent "lambda") : body') ->
        evalLambda (pure body')
    (InternalAtom (AtomIdent "define") : body') ->
        evalDefine (pure body')
    (InternalAtom (AtomIdent "if") : body') ->
        evalIf (pure body')
    content ->
        evalProcCall $ evalInternal (pure content)


evalLambda :: EvaluatorT [Internal] Internal
evalLambda content = content >>= \case
    InternalAtom (AtomIdent i) : body ->
        pure $ InternalProc (MultiArg i) body
    InternalList argExprList : body  -> do
        let identList :: Either Error [Ident]
            identList = sequence $ map ident argExprList
        let makeArgs :: [Ident] -> Args Ident
            makeArgs [] = NoArg
            makeArgs (a:rest) = Arg a $ makeArgs rest
        env <- lift get
        case makeArgs <$> identList of
            Left e -> throwE e
            Right args -> pure $ InternalProc args body
    _ ->
        throwE $ EvalError "Improper arguments to lambda"


evalDefine :: EvaluatorT [Internal] Internal
evalDefine content = content >>= \case
    InternalAtom (AtomIdent i) : value : [] -> do
        evaluated <- evalOneInternal (pure value)
        prevEnv <- lift get
        let newEnv = Map.insert i evaluated prevEnv
        let injectSelf proc@(InternalProc _ _) =
                InternalClosure newEnv proc
            injectSelf internal = internal
        lift $ put newEnv
        return $ injectSelf evaluated


evalIf :: EvaluatorT [Internal] Internal
evalIf content = content >>= \case
    (cond : thenClause : elseClause : []) -> do
        condResult <- evalOneInternal (pure cond)
        case condResult of
            InternalAtom (AtomSpecial "f") ->
                evalOneInternal (pure elseClause)
            _ ->
                evalOneInternal (pure thenClause)
    _ ->
        throwE $ EvalError "Improper use of if"


evalProcCall :: EvaluatorT [Internal] Internal
evalProcCall content = content >>= \case
    InternalProc procArgs procBody : callArgs -> do
        args <- either throwE pure (formatArgs procArgs callArgs)
        currentEnv <- lift get
        lift $ modify (injectArgs args)
        result <- evalInternal (pure procBody)
        lift $ put currentEnv
        return $ last result
    InternalBuiltInProc args func : callArgs ->
        let format (x:xs) (Arg _ rest) = (:) <$> pure x <*> format xs rest
            format [] NoArg = pure []
            format xs (MultiArg _) = pure xs
            format _ _ = Left $ EvalError "Wrong arguments"
        in either throwE pure $ applyFunc func =<< format callArgs args
    unexpected ->
        throwE $ EvalError ("Invalid procedure call: Unexpected " ++ show unexpected)


ident :: Internal -> Either Error Ident
ident (InternalAtom (AtomIdent i)) = Right i
ident _ = Left $ EvalError "Expected identificator"


lookupEnv :: EvaluatorT Ident Internal
lookupEnv name = do
    env <- lift get
    name' <- name
    case Map.lookup name' env of
        Nothing -> throwE $ EvalError ("Undefined reference " ++ name')
        Just x -> pure x


formatArgs :: Args Ident -> [Internal] -> Either Error (Args (Ident, Internal))
formatArgs NoArg [] = pure NoArg
formatArgs (MultiArg name) args = pure $ MultiArg (name, InternalList args)
formatArgs (Arg name rest) (a:args) = Arg (name, a) <$> formatArgs rest args
formatArgs _ _ = Left $ EvalError "Invalid arguments to proc"


injectArgs :: Args (Ident, Internal) -> Env -> Env
injectArgs NoArg env = env
injectArgs (MultiArg (key, val)) env  = Map.insert key val env
injectArgs (Arg (key, val) rest) env  = Map.insert key val $ injectArgs rest env


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
    AtomTokenSpecial s -> ExprAtom $ AtomSpecial s


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
