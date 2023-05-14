module Main where

import Agol.Abs
import Agol.ErrM
import Agol.Lex
import Agol.Par
import Agol.Print
import Interpreter.Interpreter
import TypeChecker.TypeChecker
import System.IO
import System.Directory.Internal.Prelude (exitFailure)
import System.Environment (getArgs)

go :: [Char] -> IO ()
go s = case pProgram (myLexer s) of
    Ok myProg -> do
        let checkRes = checkProgram myProg
        case checkRes of
            Left err ->  do 
                hPutStrLn stderr ("Static type check error! " ++ err)
            Right _ -> execProgram myProg
    Left err -> hPutStrLn stderr err

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            hSetBuffering stdout NoBuffering
            s <- getContents
            go s
        [file] -> do
            s <- readFile file
            go s