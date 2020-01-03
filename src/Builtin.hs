module Builtin where

import Expression


add :: [Expr] -> Expr
add [Number num1, Number num2] = Number (num1 + num2)

sub :: [Expr] -> Expr
sub [Number num1, Number num2] = Number (num1 - num2)

env :: [(String, Expr)]
env = [ ("+", Func add)
      , ("-", Func sub)
      ]

symbols :: [String]
symbols = map fst env