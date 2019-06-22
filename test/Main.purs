module Test.Main where

import Prelude

import Data.Lens (view, set)
import Data.Lens.Remap (remap)
import Data.Symbol (SProxy)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Type.Data.Row (RProxy(..))

p :: forall r. RProxy r
p = RProxy

main :: Effect Unit
main = do
  logShow $ view l s
  logShow $ set l b s
  where
  l = remap (p :: _ ("foo" :: SProxy "bar"))
  s = { foo: 42, baz: "quux" }
  b = { bar: "potato" }
