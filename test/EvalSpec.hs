module EvalSpec where

import Test.Hspec
import Eval
import Expression
import Builtin

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test evaluation" $ do
    it "test a simple addition" $
        eval (List [Symbol "+", Number 2, Number 3]) `shouldBe` Number 5
    it "test a simple substraction" $
        eval (List [Symbol "-", Number 2, Number 3]) `shouldBe` Number (-1)
