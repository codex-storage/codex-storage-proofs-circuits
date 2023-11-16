
-- | Merkle tree built from Poseidon2 permutation
--
-- Note: to avoid second preimage attacks, we use a keyed permutations 
-- with 2 bits of key:
--
-- * The lowest bit is set to 1 if it's the bottom layer and 0 otherwise
--
-- * The next bit is set to 1 if it's an odd node (1 child) and 0 if 
--   if it's an even node (2 children)
--

{-# LANGUAGE BangPatterns #-}
module Poseidon2.Merkle where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits

import Control.Monad

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

{-
calcMerkleTree' :: [Fr] -> [[Fr]]
calcMerkleTree' = go where
  go :: [Fr] -> [[Fr]]
  go []  = error "calcMerkleTree': input is empty"
  go [x] = [[x]]
  go xs  = xs : go (map compressPair $ pairs xs)
-}

calcMerkleTree' :: [Fr] -> [[Fr]]
calcMerkleTree' input = 
  case input of
    []  -> error "calcMerkleRoot: input is empty"
    [z] -> [[keyedCompression (nodeKey BottomLayer OddNode) z 0]]
    zs  -> go layerFlags zs 
  where
    go :: [LayerFlag] -> [Fr] -> [[Fr]]
    go _ [x]     = [[x]]
    go (f:fs) xs = xs : go fs (map (evenOddCompressPair f) $ eiPairs xs)

calcMerkleTree :: [Fr] -> MerkleTree
calcMerkleTree = MkMerkleTree . go1 . calcMerkleTree' where
  go1 outer = listArray (0, length outer-1) (map go2 outer)
  go2 inner = listArray (0, length inner-1) inner

--------------------------------------------------------------------------------

data MerkleProof = MkMerkleProof
  { _leafIndex   :: Int          -- ^ linear index of the leaf we prove, 0..dataSize-1
  , _leafData    :: Fr           -- ^ the data on the leaf
  , _merklePath  :: [Fr]         -- ^ the path up the root
  , _dataSize    :: Int          -- ^ number of leaves in the tree
  }
  deriving (Eq,Show)

arrayLength :: Array Int t -> Int
arrayLength arr = (b - a + 1) where (a,b) = bounds arr

-- | Returns the leaf and Merkle path of the given leaf
extractMerkleProof :: MerkleTree -> Int -> MerkleProof
extractMerkleProof tree@(MkMerkleTree outer) idx = MkMerkleProof idx leaf path size where
  leaf  = (outer!0)!idx
  size  = arrayLength (outer!0)
  depth = depthOf tree
  path  = worker depth idx

  worker 0     0 = []
  worker 0     _ = error "extractMerkleProof: this should not happen"
  worker level j = this : worker (level-1) (shiftR j 1) where
    this = outer ! (depth-level) ! (j `xor` 1)

extractMerkleProof_ :: MerkleTree -> Int -> [Fr]
extractMerkleProof_ tree idx = _merklePath (extractMerkleProof tree idx)

reconstructMerkleRoot :: MerkleProof -> Fr
reconstructMerkleRoot (MkMerkleProof idx leaf path size) = go layerFlags size idx leaf path where
  go _      !sz  0 !h []      = h
  go (f:fs) !sz !j !h !(p:ps) = case (j.&.1, j==sz-1)  of
    (0, False) -> go fs sz' j' (evenOddCompressPair f $ Right (h,p)) ps
    (0, True ) -> go fs sz' j' (evenOddCompressPair f $ Left   h   ) ps
    (1, _    ) -> go fs sz' j' (evenOddCompressPair f $ Right (p,h)) ps
    where
      sz' = shiftR (sz+1) 1
      j'  = shiftR  j     1

{-
reconstructMerkleRoot :: MerkleProof -> Fr
reconstructMerkleRoot (MkMerkleProof idx leaf path) = go idx leaf path where
  go  0 !h []      = h
  go !j !h !(p:ps) = case j .&. 1 of
    0 -> go (shiftR j 1) (compression h p) ps
    1 -> go (shiftR j 1) (compression p h) ps
-}

--------------------------------------------------------------------------------

testAllMerkleProofs :: Int -> IO ()
testAllMerkleProofs nn = forM_ [1..nn] $ \k -> do
  let ok = if testMerkleProofs k then "OK." else "FAILED!!"
  putStrLn $ "testing Merkle proofs for a tree with " ++ show k ++ " leaves: " ++ ok

testMerkleProofs :: Int -> Bool
testMerkleProofs = and . testMerkleProofs'

testMerkleProofs' :: Int -> [Bool]
testMerkleProofs' n = oks where
  input = map fromIntegral [1001..1000+n] :: [Fr]
  tree  = calcMerkleTree input
  root  = merkleRootOf tree
  oks   = [ reconstructMerkleRoot prf == root 
          | j<-[0..n-1]
          , let prf = extractMerkleProof tree j 
          ] 

--------------------------------------------------------------------------------

data LayerFlag  
  = BottomLayer           -- ^ it's the bottom (initial, widest) layer
  | OtherLayer            -- ^ it's not the bottom layer 
  deriving (Eq,Show)

data NodeParity
  = EvenNode              -- ^ it has 2 children
  | OddNode               -- ^ it has 1 child
  deriving (Eq,Show)

-- | Key based on the node type: 
-- 
-- > bit0 := 1 if bottom layer, 0 otherwise
-- > bit1 := 1 if odd, 0 if even
--
nodeKey :: LayerFlag -> NodeParity -> Fr
nodeKey OtherLayer  EvenNode = 0x00
nodeKey BottomLayer EvenNode = 0x01
nodeKey OtherLayer  OddNode  = 0x02
nodeKey BottomLayer OddNode  = 0x03

evenOddCompressPair :: LayerFlag -> Either Fr (Fr,Fr) -> Fr 
evenOddCompressPair !lf (Right (x,y)) = keyedCompression (nodeKey lf EvenNode) x y
evenOddCompressPair !lf (Left   x   ) = keyedCompression (nodeKey lf OddNode ) x 0

layerFlags :: [LayerFlag]    
layerFlags = BottomLayer : repeat OtherLayer 

calcMerkleRoot :: [Fr] -> Fr
calcMerkleRoot input = 
  case input of
    []  -> error "calcMerkleRoot: input is empty"
    [z] -> keyedCompression (nodeKey BottomLayer OddNode) z 0
    zs  -> go layerFlags zs 
  where
    go :: [LayerFlag] -> [Fr] -> Fr
    go _      [x] = x
    go (f:fs) xs  = go fs (map (evenOddCompressPair f) $ eiPairs xs)

--------------------------------------------------------------------------------

type Key = Fr

keyedCompressPair :: Key -> (Fr,Fr) -> Fr 
keyedCompressPair !key (!x,!y) = keyedCompression key x y

keyedCompression :: Key -> Fr -> Fr -> Fr 
keyedCompression !key !x !y = case permutation (x,y,key) of (z,_,_) -> z

eiPairs :: [Fr] -> [Either Fr (Fr,Fr)]
eiPairs []         = []
eiPairs [x]        = Left   x    : []
eiPairs (x:y:rest) = Right (x,y) : eiPairs rest

--------------------------------------------------------------------------------

compressPair :: (Fr,Fr) -> Fr 
compressPair (x,y) = compression x y

compression :: Fr -> Fr -> Fr 
compression x y = case permutation (x,y,0) of (z,_,_) -> z

{-
pairs :: [Fr] -> [(Fr,Fr)]
pairs []         = []
pairs [x]        = (x,0) : []
pairs (x:y:rest) = (x,y) : pairs rest
-}

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
