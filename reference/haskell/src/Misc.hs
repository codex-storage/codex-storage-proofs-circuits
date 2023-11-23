
module Misc where

--------------------------------------------------------------------------------

import Data.Bits

--------------------------------------------------------------------------------

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Integer -> Int
ceilingLog2 0 = 0
ceilingLog2 n = 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

--------------------------------------------------------------------------------
