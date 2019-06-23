module Data.Lens.Record.Extract where

import Data.Function (const)
import Data.Lens (Lens, lens)
import Data.Lens (view, set) as L
import Internal (rp2rlp)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.RowList (Nil, Cons, kind RowList)
import Record (get, insert, delete)
import Type.Data.RowList (RLProxy(..))
import Type.Row (class Cons, class Lacks, class RowToList) as R

class Extract (rl :: RowList) (s :: # Type) (t :: # Type) (a :: # Type) (b :: # Type) | rl s -> a, rl a -> s, rl b -> t, rl t -> b
  where
  extractRL :: forall proxy. proxy rl -> Lens { | s } { | t } { | a } { | b }

instance extractNil :: Extract Nil s s () ()
  where
  extractRL _ = lens (const {}) const

instance extractCons ::
  ( IsSymbol k

  , R.Cons k x s' s, R.Lacks k s'
  , R.Cons k x a' a, R.Lacks k a'
  , R.Cons k y t' t, R.Lacks k t'
  , R.Cons k y b' b, R.Lacks k b'

  , Extract r' s' t' a' b'
  ) =>
  Extract (Cons k v r') s t a b
  where
  extractRL _ = lens view set
    where
    k = SProxy :: _ k
    r' = RLProxy :: _ r'
    rec = extractRL r'

    view s = insert k x (L.view rec s')
      where
      x = get k s
      s' = delete k s

    set s a = insert k x (L.set rec a' s')
      where
      x = get k a
      a' = delete k a
      s' = delete k s

extracted :: forall r rl s t a b proxy. R.RowToList r rl => Extract rl s t a b => proxy r -> Lens { | s } { | t } { | a } { | b }
extracted r = extractRL (rp2rlp r)
