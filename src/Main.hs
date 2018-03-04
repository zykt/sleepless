module Main where


import Control.Monad
import Evaluator


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
    let evaluation = eval defaultEnv str
    let newEnv = either (const env) snd evaluation
    case eval defaultEnv str of
        Left e ->
            print e
        Right (result, _) ->
            forM_ result (print . simpleShowInternal)
    when (str /= "(exit)") $ repl config newEnv
