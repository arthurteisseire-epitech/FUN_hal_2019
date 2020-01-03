module Builtin where

import Expression
import Data.Fixed


add :: [Expr] -> Expr
add [Number num1, Number num2] = Number (num1 + num2)

sub :: [Expr] -> Expr
sub [Number num1, Number num2] = Number (num1 - num2)

mul :: [Expr] -> Expr
mul [Number num1, Number num2] = Number (num1 * num2)

divide :: [Expr] -> Expr
divide [Number num1, Number num2] = Number (num1 / num2)

modulo :: [Expr] -> Expr
modulo  [Number num1, Number num2] = Number (num1 `mod'` num2)

env :: [(String, Expr)]
env = [ ("+", Func add)
      , ("-", Func sub)
      , ("*", Func mul)
      , ("div", Func divide)
      , ("mod", Func modulo)
      ]

symbols :: [String]
symbols = map fst env