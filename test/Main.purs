module Test.Main where

import Prelude

import Data.Lens (Iso, re, set, view)
import Data.Lens.Remap (remap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

l = remap { "foo": SProxy :: _ "bar" }

main :: Effect Unit
main = run [consoleReporter] do

  describe "purescript-remap" do
    describe "remap" do
      let
        s = { foo: 42, baz: "quux" }
        b = { bar: "potato", baz: "quarkle" }

      it "should view correctly" do
        view l s `shouldEqual` { bar: 42, baz: "quux" }

      it "should update correctly" do
        set l b s `shouldEqual` { foo: "potato", baz: "quarkle" }

      it "should cancel out its reverse" do
        view (l <<< re l) s `shouldEqual` s
        view (re l <<< l) b `shouldEqual` b

    describe "laws" do
      -- TODO: Fill these in
      pure unit
