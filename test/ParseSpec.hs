module ParseSpec where

import Test.Hspec
import Parser
import Expression
import Builtin

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test parse" $ do
    it "test unkown symbol" $
        parse "(unknown_symbol 1 2)" `shouldBe` Left errorInParsing
    it "test invalid procedure" $
        parse "(+ toto 2)" `shouldBe` Left errorInParsing
