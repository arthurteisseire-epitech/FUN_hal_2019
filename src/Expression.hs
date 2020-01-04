module Expression
    ( Expr(..)
    ) where

data Expr
    = Number Float
    | Boolean Bool
    | Symbol String
    | Func ([Expr] -> Expr)
    | List [Expr]

instance Show Expr where
    show (Number x) = show x
    show (Symbol x) = x
    show (Boolean x) = if x then "#t" else "#f"
    show (Func x)   = "<function>"
    show (List x)   = "(" ++ unwords (map show x) ++ ")"

instance Eq Expr where
    Number x1 == Number x2 = x1 == x2
    Boolean x1 == Boolean x2 = x1 == x2
    Symbol x1 == Symbol x2 = x1 == x2
    List x1 == List x2 = x1 == x2
