module Builtins where


import Evaluator
import qualified Data.Map as Map


defaultEnv :: Env
defaultEnv = Map.singleton "add" (builtInBinaryProc builtInSum)
    where
        builtInBinaryProc proc = InternalBuiltInProc binaryArgs (Func2 proc)
        binaryArgs = Arg () (Arg () NoArg)


builtInSum :: Internal -> Internal -> Either Error Internal
builtInSum (InternalAtom (AtomInteger i1)) (InternalAtom (AtomInteger i2)) =
    pure $ InternalAtom (AtomInteger (i1 + i2))
builtInSum _ _ = Left $ EvalError "Unexpected argument to sum"
