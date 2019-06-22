module Test.Main where

import Prelude

import Data.Lens (view, set)
import Data.Lens.Remap (remap)
import Data.Symbol (SProxy)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Prim.RowList (Cons, Nil)
import Type.Row (RLProxy(..))

main :: Effect Unit
main =
  do
    logShow $ testView
    logShow $ testSet

  where
  l = remap (RLProxy :: RLProxy (Cons "foo" (SProxy "bar") Nil))

  testView = view l { foo: 42, baz: "quux" }

  testSet = set l { bar: 12 } { foo: 42, baz: "quux" }
