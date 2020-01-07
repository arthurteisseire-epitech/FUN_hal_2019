module Main where

import Control.Applicative
import Data.Char
import Eval
import Parser
import System.Environment
import System.IO
import Control.Monad (unless)

main :: IO ()
main = getArgs >>= interpret

interpret :: [String] -> IO ()
interpret [] = repl
interpret args = interpretFile args

interpretFile :: [String] -> IO ()
interpretFile args = openFile (head args) ReadMode >>= hGetContents >>= putStrLn . interpretLine . filter isControl

repl :: IO ()
repl = ((unless :: Bool -> IO () -> IO ()) <$> isEOF) *> (interactLine interpretLine >> repl)

interactLine :: (String -> String) -> IO ()
interactLine f = getLine >>= \l -> putStrLn (f l) >> hFlush stdout

interpretLine :: String -> String
interpretLine s = either show show (eval <$> parse s)
