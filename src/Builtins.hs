module Builtins where


import Evaluator
import qualified Data.Map as Map


defaultEnv :: Env
defaultEnv =
    Map.fromList [("add", integerBinOp (+))
                 ,("mul", integerBinOp (*))
                 ,("minus", integerBinOp (-))
                 ,("div", integerBinOp div)
                 ,(">", integerCompOp (>))
                 ,("<", integerCompOp (<))
                 ,(">=", integerCompOp (>=))
                 ,("<=", integerCompOp (<=))
                 ,("=", integerCompOp (==))
                 ,("!=", integerCompOp (/=))
                 ,("integer?", predicate integerPredicate)
                 ]


unaryArgs :: Args ()
unaryArgs = Arg () NoArg

predicate :: (Internal -> Either Error Internal) -> Internal
predicate p = InternalBuiltInProc unaryArgs $ Func1 p

integerPredicate :: Internal ->  Either Error Internal
integerPredicate (InternalAtom (AtomInteger _)) = pure $ InternalAtom (AtomSpecial "t")
integerPredicate _ = pure $ InternalAtom (AtomSpecial "f")


binaryArgs :: Args ()
binaryArgs = Arg () (Arg () NoArg)

integerBinOpFunc :: (Integer -> Integer -> Integer) -> Internal -> Internal -> Either Error Internal
integerBinOpFunc op (InternalAtom (AtomInteger i1)) (InternalAtom (AtomInteger i2)) =
    pure $ InternalAtom (AtomInteger (op i1 i2))
integerBinOpFunc _ _ _ = Left $ EvalError "Unexpected argument to int function"

integerBinOp :: (Integer -> Integer -> Integer) -> Internal
integerBinOp op = InternalBuiltInProc binaryArgs (Func2 $ integerBinOpFunc op)

integerCompOpFunc :: (Integer -> Integer -> Bool) -> Internal -> Internal -> Either Error Internal
integerCompOpFunc op (InternalAtom (AtomInteger i1)) (InternalAtom (AtomInteger i2))
    | op i1 i2 = pure $ InternalAtom (AtomSpecial "t")
    | otherwise = pure $ InternalAtom (AtomSpecial "f")

integerCompOp :: (Integer -> Integer -> Bool) -> Internal
integerCompOp op = InternalBuiltInProc binaryArgs (Func2 $ integerCompOpFunc op)
