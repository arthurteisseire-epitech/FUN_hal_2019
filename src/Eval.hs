module Eval where

import           Builtin
import           Data.Maybe
import           Expression

eval :: Expr -> Expr
eval (Number n) = Number n
eval (Symbol s) = fromJust $ lookup s env
eval (Func f) = Func f
eval (List (x:xs)) = apply (eval x) (map eval xs)
  where
    apply (Func f) = f
