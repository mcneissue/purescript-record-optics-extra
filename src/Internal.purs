module Internal where

import Type.Row (class RowToList) as R
import Type.Data.RowList (RLProxy(..))

rp2rlp :: forall r rl proxy. R.RowToList r rl => proxy r -> RLProxy rl
rp2rlp _ = RLProxy :: _ rl
