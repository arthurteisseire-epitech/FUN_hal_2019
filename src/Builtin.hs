module Builtin where

import Expression
import Data.Fixed


arithmetic :: (Float -> Float -> Float) -> [Expr] -> Expr
arithmetic f [Number num1, Number num2] = Number $ f num1 num2

env :: [(String, Expr)]
env = [ ("+", Func (arithmetic (+)))
      , ("-", Func (arithmetic (-)))
      , ("*", Func (arithmetic (*)))
      , ("div", Func (arithmetic (/)))
      , ("mod", Func (arithmetic mod'))
      ]

symbols :: [String]
symbols = map fst env