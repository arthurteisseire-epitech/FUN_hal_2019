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
interpretFile args = openFile (head args) ReadMode >>= hGetContents >>= putStrLn . interpretLine . filter isControl

repl :: IO ()
repl = do
    isEof <- isEOF
    if isEof
        then putStrLn "goodbye!"
        else do
            interactLine interpretLine
            repl

interpretLine :: String -> String
interpretLine = showEither . (fmap . fmap) eval parse

showEither :: Show a => Either String a -> String
showEither = either show show

interactLine :: (String -> String) -> IO ()
interactLine f = do
    s <- getLine
    putStrLn (f s)
    hFlush stdout
