module Builtin where

import Expression
import Data.Fixed


arithmetic :: (Float -> Float -> Float) -> [Expr] -> Expr
arithmetic op = foldl1 (f op)
  where f op (Number a) (Number b) = Number (a `op` b)

env :: [(String, Expr)]
env = [ ("+", Func (arithmetic (+)))
      , ("-", Func (arithmetic (-)))
      , ("*", Func (arithmetic (*)))
      , ("div", Func (arithmetic (/)))
      , ("mod", Func (arithmetic mod'))
      ]

symbols :: [String]
symbols = map fst env