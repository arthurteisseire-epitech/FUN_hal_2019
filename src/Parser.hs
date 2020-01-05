module Parser where

import           Builtin
import           Control.Applicative
import           Expression
import           ParseUtils
import           Text.ParserCombinators.ReadP

parse :: String -> Either String Expr
parse s
    | null res = Left errorInParsing
    | otherwise = Right $ fst $ last res
  where
    res = readP_to_S parseExpr s

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

errorInParsing :: String
errorInParsing = "Error in parsing"
