module Data.Lens.Remap where

import Prelude

import Data.Lens (Lens, lens)
import Data.Lens (view, set) as L
import Data.Symbol (class IsSymbol)
import Prim.RowList (Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.RowList (RLProxy(..))
import Type.Prelude (SProxy(..), class RowToList)
import Type.Row (class Cons, class Lacks) as R

class Remap (rl :: RowList) (s :: # Type) (t :: # Type) (a :: # Type) (b :: # Type) | rl s -> a b, b -> t
  where
  remapRL :: forall proxy. proxy rl -> Lens { | s } { | t } { | a } { | b }

instance remapNil :: Remap Nil s s () ()
  where
  remapRL r = lens (const {}) (const)

instance remapCons ::
  ( IsSymbol k
  , IsSymbol l

  , R.Cons k v s' s, R.Lacks k s'
  , R.Cons k u t' t, R.Lacks k t'
  , R.Cons l v a' a, R.Lacks l a'
  , R.Cons l u b' b, R.Lacks l b'

  , Remap r' s' t' a' b'
  ) =>
  Remap (Cons k (proxy l) r') s t a b
  where
  remapRL r = lens view update
    where
    r' = RLProxy :: _ r'
    k = SProxy :: _ k
    l = SProxy :: _ l

    view :: { | s } -> { | a }
    view s = insert l v (L.view (remapRL r') s')
      where
      v = get k s
      s' = delete k s

    update :: { | s } -> { | b } -> { | t }
    update s b = insert k u (L.set (remapRL r') b' s')
      where
      u = get l b
      s' = delete k s
      b' = delete l b

rp2rlp :: forall r rl proxy. RowToList r rl => proxy r -> RLProxy rl
rp2rlp _ = RLProxy :: _ rl

remap :: forall r rl s t a b proxy. RowToList r rl => Remap rl s t a b => proxy r -> Lens { | s } { | t } { | a } { | b }
remap r = remapRL (rp2rlp r)
