module Data.Lens.Extract where

import Data.Function (const)
import Data.Lens (Lens', lens)
import Data.Lens (view, set) as L
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.RowList (Nil, Cons, kind RowList)
import Record (get, insert, delete)
import Type.Data.RowList (RLProxy(..))
import Type.Row (class Cons, class Lacks, class RowToList) as R

class Extract (rl :: RowList) (s :: # Type) (a :: # Type) | rl s -> a, rl a -> s
  where
  extractRL :: forall proxy. proxy rl -> Lens' { | s } { | a }

instance extractNil :: Extract Nil s ()
  where
  extractRL _ = lens (const {}) const

instance extractCons ::
  ( IsSymbol k

  , R.Cons k x s' s, R.Lacks k s'
  , R.Cons k x a' a, R.Lacks k a'

  , Extract r' s' a'
  ) =>
  Extract (Cons k v r') s a
  where
  extractRL _ = lens view set
    where
    k = SProxy :: _ k
    r' = RLProxy :: _ r'
    rec = extractRL r'

    view :: { | s } -> { | a }
    view s = insert k x (L.view rec s')
      where
      x = get k s
      s' = delete k s

    set :: { | s } -> { | a } -> { | s }
    set s a = insert k x (L.set rec a' s')
      where
      x = get k a
      a' = delete k a
      s' = delete k s

rp2rlp :: forall r rl proxy. R.RowToList r rl => proxy r -> RLProxy rl
rp2rlp _ = RLProxy :: _ rl

extract :: forall r rl s a proxy. R.RowToList r rl => Extract rl s a => proxy r -> Lens' { | s } { | a }
extract r = extractRL (rp2rlp r)
