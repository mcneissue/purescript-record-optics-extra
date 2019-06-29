module Internal where

import Prim.RowList (class RowToList)
import Type.Data.RowList (RLProxy(..))

rp2rlp :: forall r rl proxy. RowToList r rl => proxy r -> RLProxy rl
rp2rlp _ = RLProxy :: _ rl
