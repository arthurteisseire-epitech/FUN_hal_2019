module Builtin where

import Expression
import Data.Fixed


arithmetic :: (Float -> Float -> Float) -> [Expr] -> Expr
arithmetic op = foldl1 (apply op)

operate :: (Float -> Float -> Float) -> [Expr] -> Expr
operate op [n1, n2] = apply op n1 n2

apply :: (Float -> Float -> Float) -> Expr -> Expr -> Expr
apply op (Number n1) (Number n2) = Number (n1 `op` n2)

inferior :: [Expr] -> Expr
inferior [Number n1, Number n2] = Boolean $ n1 < n2

eq :: [Expr] -> Expr
eq [e1, e2] = Boolean $ e1 == e2 

env :: [(String, Expr)]
env = [ ("+", Func (arithmetic (+)))
      , ("-", Func (arithmetic (-)))
      , ("*", Func (arithmetic (*)))
      , ("div", Func (arithmetic (/)))
      , ("mod", Func (operate mod'))
      , ("<", Func inferior)
      , ("eq?", Func eq)
      ]

symbols :: [String]
symbols = map fst env