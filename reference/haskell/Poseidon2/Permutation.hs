
-- | The Poseidon2 permutation

module Poseidon2.Permutation where

--------------------------------------------------------------------------------

import ZK.Algebra.Curves.BN128.Fr.Mont (Fr)

import Poseidon2.RoundConsts

--------------------------------------------------------------------------------

sbox :: Fr -> Fr
sbox x = x4*x where
  x2 = x *x
  x4 = x2*x2

internalRound :: Fr -> (Fr,Fr,Fr) -> (Fr,Fr,Fr) 
internalRound c (x,y,z) = 
  ( 2*x' +   y +   z 
  ,   x' + 2*y +   z 
  ,   x' +   y + 3*z 
  )
  where
    x' = sbox (x + c) 

externalRound :: (Fr,Fr,Fr) -> (Fr,Fr,Fr) -> (Fr,Fr,Fr)
externalRound (cx,cy,cz) (x,y,z) = (x'+s , y'+s , z'+s) where
  x' = sbox (x + cx)
  y' = sbox (y + cy)
  z' = sbox (z + cz)
  s  = x' + y' + z'

linearLayer :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
linearLayer (x,y,z) = (x+s, y+s, z+s) where s = x+y+z

--------------------------------------------------------------------------------

permutation :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
permutation 
  = (\state -> foldl (flip externalRound) state finalRoundConsts   )
  . (\state -> foldl (flip internalRound) state internalRoundConsts)
  . (\state -> foldl (flip externalRound) state initialRoundConsts )
  . linearLayer

--------------------------------------------------------------------------------
