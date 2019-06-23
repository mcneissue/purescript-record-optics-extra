module Data.Lens.Record.Remap where

import Prelude

import Data.Lens (Iso, iso, re, view)
import Internal (rp2rlp)
import Data.Symbol (class IsSymbol)
import Prim.RowList (Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.RowList (RLProxy(..))
import Type.Prelude (SProxy(..), class RowToList)
import Type.Row (class Cons, class Lacks) as R

class Remap (rl :: RowList) (s :: # Type) (t :: # Type) (a :: # Type) (b :: # Type) | rl s -> a, rl a -> s, rl b -> t, rl t -> b
  where
  remapRL :: forall proxy. proxy rl -> Iso { | s } { | t } { | a } { | b }

instance remapNil :: Remap Nil s s s s
  where
  remapRL r = iso identity identity

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
  remapRL r = iso to from
    where
    k = SProxy :: _ k
    l = SProxy :: _ l
    r' = RLProxy :: _ r'
    rec = remapRL r'

    to :: { | s } -> { | a }
    to s = insert l v (view rec s')
      where
      v = get k s
      s' = delete k s

    from :: { | b } -> { | t }
    from b = insert k u (view (re rec) b')
      where
      u = get l b
      b' = delete l b

remap :: forall r rl s t a b proxy. RowToList r rl => Remap rl s t a b => proxy r -> Iso { | s } { | t } { | a } { | b }
remap r = remapRL (rp2rlp r)
