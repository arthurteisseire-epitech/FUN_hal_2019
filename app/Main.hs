module Main where

import           Eval
import           Parser
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = getArgs >>= interpret

interpret :: [String] -> IO ()
interpret args
    | null args = repl
    | otherwise = interpretFile args

interpretFile :: [String] -> IO ()
interpretFile args = openFile (head args) ReadMode >>= hGetContents >>= interpretLine . filter (/= '\n')

repl :: IO ()
repl = do
    isEof <- isEOF
    if isEof
        then putStrLn "goodbye!"
        else do
            line <- getLine
            interpretLine line
            hFlush stdout
            repl

interpretLine :: String -> IO ()
interpretLine = print . eval . parse
