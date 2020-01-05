module Expression
    ( Expr(..)
    ) where

import           Text.Printf (printf)

data Expr
    = Number Float
    | Symbol String
    | Boolean Bool
    | Func ([Expr] -> Expr)
    | List [Expr]

instance Show Expr where
    show (Number x)
        | x == fromInteger (round x) = printf "%.0f" x
        | otherwise = show x
    show (Symbol x) = x
    show (Boolean x) = if x then "#t" else "#f"
    show (Func x) = "<function>"
    show (List x) = "(" ++ unwords (map show x) ++ ")"

instance Eq Expr where
    Number x1 == Number x2 = x1 == x2
    Symbol x1 == Symbol x2 = x1 == x2
    Boolean x1 == Boolean x2 = x1 == x2
    Func x1 == Func x2 = False
    List x1 == List x2 = x1 == x2
