module Main where


import System.IO (hFlush, stdout)
import Control.Monad
import Control.Applicative (optional)
import Data.Semigroup ((<>))
import Options.Applicative as Opt
import Evaluator
import Builtins


data Config = Config {
    target  :: Maybe String,
    debugir :: Bool
}


main :: IO ()
main = do
    config <- execParser opts
    case target config of
        Nothing ->
            repl config defaultEnv
        Just filename -> do
            file <- readFile filename
            case eval defaultEnv file of
                Left e ->
                    print e
                Right (result, _) -> do
                    print file
                    mapM_ (putStrLn . simpleShowInternal) result
    where
        opts = Opt.info (Opt.helper <*> configParser)
            (  Opt.fullDesc
            <> Opt.progDesc "Sleepless interpreter"
            <> Opt.header "Sleepless - lispy lang")


configParser :: Opt.Parser Config
configParser = Config
    <$> optional (Opt.strArgument (Opt.metavar "FILE"))
    <*> Opt.switch
        (  Opt.long "debugir"
        <> Opt.short 'i'
        <> Opt.help "Debug Internal Representation"
        )


repl :: Config -> Env -> IO ()
repl config env = do
    putStr "> "
    hFlush stdout
    str <- getLine
    when (debugir config) $ debugEval str
    let evaluation = eval env str
    let newEnv = either (const env) snd evaluation
    case evaluation of
        Left e ->
            print e
        Right (result, _) ->
            forM_ result (putStrLn . simpleShowInternal)
    when (str /= "(exit)") $ repl config newEnv
