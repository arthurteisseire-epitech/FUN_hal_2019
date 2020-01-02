module Expression
    ( Expr(..)
    ) where

data Expr
    = Number Float
    | Symbol String
    | Func ([Expr] -> Expr)
    | List [Expr]

instance Show Expr where
    show (Number x) = show x
    show (Symbol x) = x
    show (Func x)   = "<function>"
    show (List x)   = "(" ++ unwords (map show x) ++ ")"

instance Eq Expr where
    Number x1 == Number x2 = x1 == x2
    Symbol x1 == Symbol x2 = x1 == x2
    List x1 == List x2 = x1 == x2
