module Data.Lens.Remap where

import Prelude

import Data.Lens (Lens, lens)
import Data.Lens (view, set) as L
import Data.Symbol (class IsSymbol)
import Prim.RowList (Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Prelude (SProxy(..), class RowToList)
import Type.Row (class Cons, class Lacks) as R

class Remap (rl :: RowList) (s :: # Type) (t :: # Type) (a :: # Type) (b :: # Type) | rl s -> a b, b -> t
  where
  remapRL :: RLProxy rl -> Lens { | s } { | t } { | a } { | b }

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
  Remap (Cons k (SProxy l) r') s t a b
  where
  remapRL r = lens view update
    where
    view :: { | s } -> { | a }
    view s = insert l v (L.view (remapRL r') s')
      where
      v :: v
      v = get k s

      s' :: { | s' }
      s' = delete k s

    update :: { | s } -> { | b } -> { | t }
    update s b = insert k u (L.set (remapRL r') b' s')
      where
      u :: u
      u = get l b

      s' :: { | s' }
      s' = delete k s

      b' :: { | b' }
      b' = delete l b

    r' = RLProxy :: _ r'

    k = SProxy :: _ k

    l = SProxy :: _ l

rp2rlp :: forall r rl. RowToList r rl => RProxy r -> RLProxy rl
rp2rlp _ = RLProxy :: _ rl

remap :: forall r rl s t a b. RowToList r rl => Remap rl s t a b => RProxy r -> Lens { | s } { | t } { | a } { | b }
remap r = remapRL (rp2rlp r)
