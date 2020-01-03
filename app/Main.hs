module Main where

import Parser
import Eval

main :: IO ()
main = do
    line <- getLine
    print $ (eval . parse) line
