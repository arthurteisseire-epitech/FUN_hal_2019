module Main where

import           Data.Char
import           Eval
import           Parser
import           System.Environment
import           System.IO

main :: IO ()
main = getArgs >>= interpret

interpret :: [String] -> IO ()
interpret args
    | null args = repl
    | otherwise = interpretFile args

interpretFile :: [String] -> IO ()
interpretFile args = openFile (head args) ReadMode >>= hGetContents >>= interpretLine . filter isControl

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
interpretLine s = either print print (eval <$> parse s)
