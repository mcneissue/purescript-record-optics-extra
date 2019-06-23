module Test.Main where

import Prelude

import Data.Lens (re, set, view)
import Data.Lens.Extract (extract)
import Data.Lens.Remap (remap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

scheme = { "foo": bar }
  where
  bar = SProxy :: _ "bar"

remapped = remap scheme
extracted = extract scheme
both = remapped >>> extracted

main :: Effect Unit
main = run [consoleReporter] do

  describe "purescript-remap" do
    let
      s = { foo: 42, baz: "quux" }
      b = { bar: "potato", baz: "quarkle" }

    describe "remap" do
      it "should view" do
        view remapped s `shouldEqual` { bar: 42, baz: "quux" }

      it "should update" do
        set remapped b s `shouldEqual` { foo: "potato", baz: "quarkle" }

      it "should cancel out its reverse" do
        view (remapped <<< re remapped) s `shouldEqual` s
        view (re remapped <<< remapped) b `shouldEqual` b

    describe "extract" do
      it "should extract subrecords" do
        view extracted s `shouldEqual` { foo: 42 }

      it "should update using subrecords" do
        set extracted { foo: "potato" } s `shouldEqual` { foo: "potato", baz: "quux" }

    describe "composition" do
      it "should work properly" do
        view both s `shouldEqual` { bar: 42 }
        set both { bar: 21 } s `shouldEqual` { foo: 21, baz: "quux" }

    describe "laws" do
      -- TODO: Fill these in
      pure unit
