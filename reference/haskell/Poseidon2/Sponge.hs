
module Poseidon2.Sponge where

--------------------------------------------------------------------------------

import ZK.Algebra.Curves.BN128.Fr.Mont (Fr)

import Poseidon2.Permutation

--------------------------------------------------------------------------------

-- | Sponge construction with rate=1 (capacity=2), zero IV and 10* padding
sponge1 :: [Fr] -> Fr
sponge1 input = go (0,0,0) (pad input) where

  pad :: [Fr] -> [Fr]
  pad (x:xs) = x : pad xs
  pad []     = [1]

  go (sx,_ ,_ ) []     = sx
  go (sx,sy,sz) (a:as) = go state' as where 
    state' = permutation (sx+a, sy, sz)

--------------------------------------------------------------------------------

-- | Sponge construction with rate=2 (capacity=1), zero IV and 10* padding
sponge2 :: [Fr] -> Fr
sponge2 input = go (0,0,0) (pad input) where

  pad :: [Fr] -> [Fr]
  pad (x:y:rest) = x : y : pad rest
  pad [x]        = [x,1]
  pad []         = [1,0]

  go (sx,_ ,_ ) []         = sx
  go (sx,sy,sz) (a:b:rest) = go state' rest where 
    state' = permutation (sx+a, sy+b, sz)

--------------------------------------------------------------------------------

