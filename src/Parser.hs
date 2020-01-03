module Parser where

import           Builtin
import           Control.Applicative
import           Expression
import           ParseUtils
import           Text.ParserCombinators.ReadP

parse :: String -> Expr
parse s = fst $ last $ readP_to_S parseExpr s

parseExpr :: ReadP Expr
parseExpr = skipSpaces *> parseExprHelper <* skipSpaces

parseExprHelper :: ReadP Expr
parseExprHelper = decimal <|> symbol <|> parseList

parseList :: ReadP Expr
parseList = do
    char '('
    expr <- skipSpaces *> endBy parseExprHelper skipSpaces <* skipSpaces
    char ')'
    return $ List expr

symbol :: ReadP Expr
symbol = Symbol <$> choice (map string symbols)

decimal :: ReadP Expr
decimal = Number . rd <$> integer <++> decimalPart <++> e
  where
    rd = read :: String -> Float
    decimalPart = option "" (char '.' <:> number)
    e = option "" (oneOf "eE" <:> integer)

oneOf :: String -> ReadP Char
oneOf s = satisfy (`elem` s)

charToString :: Char -> String
charToString c = [c]
