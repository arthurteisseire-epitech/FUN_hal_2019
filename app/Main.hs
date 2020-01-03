module Main where

import           Eval
import           Parser
import           System.Exit
import           System.IO

main :: IO ()
main = repl

repl :: IO ()
repl = do
    isEof <- isEOF
    if isEof
        then exitSuccess
        else do
            line <- getLine
            print $ (eval . parse) line
            hFlush stdout
            repl
