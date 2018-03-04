module Main where


import Control.Monad
import Evaluator
import Builtins


data Config = Config { debug :: Bool }


main :: IO ()
main = do
    let config = Config False
    repl config defaultEnv


repl :: Config -> Env -> IO ()
repl config env = do
    putStr "> "
    str <- getLine
    when (debug config) $ debugEval str
    let evaluation = eval env str
    let newEnv = either (const env) snd evaluation
    case evaluation of
        Left e ->
            print e
        Right (result, _) ->
            forM_ result (print . simpleShowInternal)
    when (str /= "(exit)") $ repl config newEnv
