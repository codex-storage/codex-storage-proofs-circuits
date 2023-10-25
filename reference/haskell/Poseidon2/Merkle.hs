
-- | Merkle tree built from Poseidon2 permutation

{-# LANGUAGE BangPatterns #-}
module Poseidon2.Merkle where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits

import ZK.Algebra.Curves.BN128.Fr.Mont (Fr)

import Poseidon2.Permutation

-- import Debug.Trace
-- debug s x y = trace (s ++ " ~> " ++ show x) y

--------------------------------------------------------------------------------

-- | A Merkle tree. 
--
-- Note the first layer is the bottom (widest) layer, and the last layer is the top (root).
--
newtype MerkleTree 
  = MkMerkleTree (Array Int (Array Int Fr))
  deriving Show

merkleRootOf :: MerkleTree -> Fr
merkleRootOf (MkMerkleTree outer) 
  | c == d     = inner ! c
  | otherwise  = error "merkleRootOf: topmost layer is not singleton"
  where
    (a,b) = bounds outer
    inner = outer ! b
    (c,d) = bounds inner

-- | @log2( number-of-leaves )@.
-- 
-- NOTE: this is one less than the actual number of layers!
--
depthOf :: MerkleTree -> Int
depthOf (MkMerkleTree outer) = b-a where
  (a,b) = bounds outer

calcMerkleTree' :: [Fr] -> [[Fr]]
calcMerkleTree' = go where
 go :: [Fr] -> [[Fr]]
 go []  = error "calcMerkleTree': input is empty"
 go [x] = [[x]]
 go xs  = xs : go (map compressPair $ pairs xs)

calcMerkleTree :: [Fr] -> MerkleTree
calcMerkleTree = MkMerkleTree . go1 . calcMerkleTree' where
  go1 outer = listArray (0, length outer-1) (map go2 outer)
  go2 inner = listArray (0, length inner-1) inner

data MerkleProof = MkMerkleProof
  { _leafIndex  :: Int
  , _leafHash   :: Fr
  , _merklePath :: [Fr]
  }
  deriving (Eq,Show)

-- | Returns the leaf and Merkle path of the given leaf
extractMerkleProof :: MerkleTree -> Int -> MerkleProof
extractMerkleProof tree@(MkMerkleTree outer) idx = MkMerkleProof idx leaf path where
  leaf  = (outer!0)!idx
  depth = depthOf tree
  path  = worker depth idx

  worker 0     0 = []
  worker 0     _ = error "extractMerkleProof: this should not happen"
  worker level j = this : worker (level-1) (shiftR j 1) where
    this = outer ! (depth-level) ! (j `xor` 1)

extractMerkleProof_ :: MerkleTree -> Int -> [Fr]
extractMerkleProof_ tree idx = _merklePath (extractMerkleProof tree idx)

reconstructMerkleRoot :: MerkleProof -> Fr
reconstructMerkleRoot (MkMerkleProof idx leaf path) = go idx leaf path where
  go  0 !h []      = h
  go !j !h !(p:ps) = case j .&. 1 of
    0 -> go (shiftR j 1) (compression h p) ps
    1 -> go (shiftR j 1) (compression p h) ps

--------------------------------------------------------------------------------

calcMerkleRoot :: [Fr] -> Fr
calcMerkleRoot = go where
  go []  = error "calcMerkleRoot: input is empty"
  go [x] = x
  go xs  = go (map compressPair $ pairs xs)

compressPair :: (Fr,Fr) -> Fr 
compressPair (x,y) = compression x y

compression :: Fr -> Fr -> Fr 
compression x y = case permutation (x,y,0) of (z,_,_) -> z

pairs :: [Fr] -> [(Fr,Fr)]
pairs []         = []
pairs [x]        = (x,x) : []
pairs (x:y:rest) = (x,y) : pairs rest

--------------------------------------------------------------------------------

printExampleMerkleRoots :: IO ()
printExampleMerkleRoots = do
  putStrLn $ "Merkle root for [1..   1] = " ++ show (calcMerkleRoot $ map fromInteger [1..   1])
  putStrLn $ "Merkle root for [1..   2] = " ++ show (calcMerkleRoot $ map fromInteger [1..   2])
  putStrLn $ "Merkle root for [1..   4] = " ++ show (calcMerkleRoot $ map fromInteger [1..   4])
  putStrLn $ "Merkle root for [1..  16] = " ++ show (calcMerkleRoot $ map fromInteger [1..  16])
  putStrLn $ "Merkle root for [1..  64] = " ++ show (calcMerkleRoot $ map fromInteger [1..  64])
  putStrLn $ "Merkle root for [1.. 256] = " ++ show (calcMerkleRoot $ map fromInteger [1.. 256])
  putStrLn $ "Merkle root for [1..1024] = " ++ show (calcMerkleRoot $ map fromInteger [1..1024])

--------------------------------------------------------------------------------
