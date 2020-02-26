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
interpret args = mapM_ interpretFile args

interpretFile :: String -> IO ()
interpretFile filename = readFile filename >>= putStrLn . interpretLine . filter (not . isControl)

repl :: IO ()
repl = unlessM isEOF (interactLine interpretLine >> repl)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b m = b >>= (`unless` m)

interactLine :: (String -> String) -> IO ()
interactLine f = getLine >>= putStrLn . f >> hFlush stdout

interpretLine :: String -> String
interpretLine s = either show show (eval <$> parse s)
