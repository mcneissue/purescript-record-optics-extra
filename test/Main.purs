module Test.Main where

import Prelude

import Data.Lens (view, set)
import Data.Lens.Remap (remap)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Type.Data.Row (RProxy(..))

p :: forall r. RProxy r
p = RProxy


main :: Effect Unit
main = run [consoleReporter] do
  let
    l = remap (p :: _ ("foo" :: _ "bar"))
    s = { foo: 42, baz: "quux" }
    b = { bar: "potato" }

  describe "purescript-remap" do

    describe "happy path" do

      it "should view correctly" do
        (view l s) `shouldEqual` { bar: 42 }

      it "should update correctly" do
        (set l b s) `shouldEqual` { foo: "potato", baz: "quux" }
