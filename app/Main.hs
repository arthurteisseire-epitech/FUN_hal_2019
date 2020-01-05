module Main where

import           Data.Char
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
interpretLine s =
    case eval <$> parse s of
        Left s  -> print s
        Right s -> print s
