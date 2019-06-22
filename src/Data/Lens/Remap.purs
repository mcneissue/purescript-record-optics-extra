module Data.Lens.Remap where

import Prelude

import Data.Lens (Lens, lens)
import Data.Lens (view, set) as L
import Data.Symbol (class IsSymbol)
import Prim.RowList (Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.RowList (RLProxy(..))
import Type.Prelude (SProxy(..))
import Type.Row (class Cons, class Lacks) as R

class Remap (rl :: RowList) (s :: # Type) (t :: # Type) (a :: # Type) (b :: # Type) | rl s -> a b, b -> t
  where
  remap       :: RLProxy rl -> Lens { | s } { | t } { | a } { | b }

instance remapNil :: Remap Nil s s () ()
  where
  remap r = lens (const {}) (const)

instance remapCons ::
  ( IsSymbol k
  , IsSymbol l

  , R.Cons k v s' s, R.Lacks k s'
  , R.Cons k u t' t, R.Lacks k t'
  , R.Cons l v a' a, R.Lacks l a'
  , R.Cons l u b' b, R.Lacks l b'

  , Remap r' s' t' a' b'
  ) =>
  Remap (Cons k (SProxy l) r') s t a b
  where

  remap r = lens (view r) (update r)
    where
    r' :: RLProxy r'
    r' = RLProxy

    k :: SProxy k
    k = SProxy

    l :: SProxy l
    l = SProxy

    view :: (RLProxy (Cons k (SProxy l) r')) -> { | s } -> { | a }
    view _ s = insert l v (L.view (remap r') s')
      where
      v :: v
      v = get k s

      s' :: { | s' }
      s' = delete k s

    update :: (RLProxy (Cons k (SProxy l) r')) -> { | s } -> { | b } -> { | t }
    update _ s b = insert k u (L.set (remap r') b' s')
      where
      u :: u
      u = get l b

      s' :: { | s' }
      s' = delete k s

      b' :: { | b' }
      b' = delete l b
