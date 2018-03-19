module Builtins where


import Evaluator
import qualified Data.Map as Map


defaultEnv :: Env
defaultEnv = --Map.singleton "add" (builtInBinaryProc builtInSum)
    Map.fromList [("add", integerBinOp (+))
                 ,("mul", integerBinOp (*))
                 ,("minus", integerBinOp (-))
                 ,("div", integerBinOp div)
                 ]


binaryArgs :: Args ()
binaryArgs = Arg () (Arg () NoArg)

integerBinOpFunc :: (Integer -> Integer -> Integer) -> Internal -> Internal -> Either Error Internal
integerBinOpFunc op (InternalAtom (AtomInteger i1)) (InternalAtom (AtomInteger i2)) =
    pure $ InternalAtom (AtomInteger (op i1 i2))
integerBinOpFunc _ _ _ = Left $ EvalError "Unexpected argument to int function"

integerBinOp :: (Integer -> Integer -> Integer) -> Internal
integerBinOp op = InternalBuiltInProc binaryArgs (Func2 $ integerBinOpFunc op)
