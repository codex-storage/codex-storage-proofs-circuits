
-- | Poseidon2 hash function for the bn128 curve's scalar field and t=3.

module Poseidon2 
  ( Fr
  , sponge1 , sponge2
  , calcMerkleRoot , calcMerkleTree
  , MerkleTree(..) , depthOf , merkleRootOf
  , MerkleProof(..) , extractMerkleProof , extractMerkleProof_ , reconstructMerkleRoot
  , compressPair, keyedCompressPair
  , permutation
  ) 
  where

--------------------------------------------------------------------------------

import ZK.Algebra.Curves.BN128.Fr.Mont (Fr)

import Poseidon2.Sponge
import Poseidon2.Merkle
import Poseidon2.Permutation

--------------------------------------------------------------------------------
