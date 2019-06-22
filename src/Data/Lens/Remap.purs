module Data.Lens.Remap where

import Data.Lens (Lens', lens)
import Data.Symbol (class IsSymbol)
import Prim.RowList (Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.RowList (RLProxy(..))
import Type.Prelude (SProxy(..))
import Type.Row (class Cons, class Lacks) as R

class Remap (rl :: RowList) (i :: # Type) (o :: # Type) | rl i -> o
  where
  remapView :: RLProxy rl -> { | i } -> { | o }
  remapUpdate :: RLProxy rl -> { | i } -> { | o } -> { | i }
  remap :: RLProxy rl -> Lens' { | i } { | o }

instance remapNil :: Remap Nil a ()
  where
  remapView _ _ = {}
  remapUpdate _ s _ = s
  remap r = lens (remapView r) (remapUpdate r)

instance remapCons ::
  ( IsSymbol k
  , IsSymbol l

  , R.Cons k v i' i, R.Lacks k i'
  , R.Cons l v o' o, R.Lacks l o'

  , Remap r' i' o'
  ) =>
  Remap (Cons k (SProxy l) r') i o
  where

  remapView _ i = insert l v (remapView r' i')
    where
    r' :: RLProxy r'
    r' = RLProxy

    k :: SProxy k
    k = SProxy

    l :: SProxy l
    l = SProxy

    v :: v
    v = get k i

    i' :: { | i' }
    i' = delete k i

  remapUpdate _ i o = insert k v (remapUpdate r' i' o')
    where
    r' :: RLProxy r'
    r' = RLProxy

    k :: SProxy k
    k = SProxy

    l :: SProxy l
    l = SProxy

    v :: v
    v = get l o

    i' :: { | i' }
    i' = delete k i

    o' :: { | o' }
    o' = delete l o

  remap r = lens (remapView r) (remapUpdate r)
