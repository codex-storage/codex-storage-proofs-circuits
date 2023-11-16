
module Sampling where

--------------------------------------------------------------------------------

import Control.Monad
import System.IO

import Poseidon2
import Slot  

import qualified ZK.Algebra.Curves.BN128.Fr.Mont as Fr

--------------------------------------------------------------------------------

samplingTest :: SlotConfig -> FilePath -> IO ()
samplingTest slotCfg fpath = do
  let entropy = 123456789 :: Fr
  input <- calculateCircuitInput slotCfg entropy
  exportCircuitInput fpath input

--------------------------------------------------------------------------------

type Entropy = Fr

-- | Given an entropy source, the slot root, and a counter, we compute a
-- cell index to sample
sampleCellIndex :: SlotConfig -> Entropy -> Hash -> Int -> Int
sampleCellIndex cfg entropy slotRoot counter = fromInteger idx where
  u   = sponge2 [entropy , slotRoot , fromIntegral counter] :: Fr
  idx = (Fr.from u) `mod` n          :: Integer
  n   = (fromIntegral $ _nCells cfg) :: Integer
 
--------------------------------------------------------------------------------

data CircuitInput = MkInput 
  { _entropy     :: Entropy       -- ^ public input
  , _slotRoot    :: Hash          -- ^ public input
  , _cellData    :: [[Fr]]        -- ^ private input
  , _merklePaths :: [[Fr]]        -- ^ private input
  }
  deriving Show

-- | Calculate the the inputs for the storage proof circuit
calculateCircuitInput :: SlotConfig -> Entropy -> IO CircuitInput
calculateCircuitInput slotCfg entropy = do
  slotTree <- calcSlotTree slotCfg 
  let slotRoot = merkleRootOf slotTree 
  let idxs = [ sampleCellIndex slotCfg entropy slotRoot j | j <- [1..(_nSamples slotCfg)] ]
  cellData <- forM idxs $ \idx -> (cellDataToFieldElements <$> loadCellData slotCfg idx)
  let merklePaths = [ extractMerkleProof_ slotTree idx | idx <- idxs ]
  return $ MkInput
    { _entropy    =  entropy
    , _slotRoot    = slotRoot
    , _cellData    = cellData
    , _merklePaths = merklePaths
    }

-- | Export the inputs of the storage proof circuits in JSON format,
-- which @circom@ can consume.
--
-- NOTE: large numbers (field elements) must be encoded as JSON strings,
-- not numbers, as Javascript cannot handle large numbers!
--
exportCircuitInput :: FilePath -> CircuitInput -> IO ()
exportCircuitInput fpath input = do
  h <- openFile fpath WriteMode
  hPutStrLn h $ "{ \"entropy\":  " ++ show (show (_entropy  input))
  hPutStrLn h $ ", \"slotRoot\": " ++ show (show (_slotRoot input))
  hPutStrLn h $ ", \"cellData\": " 
  hPrintListOfLists h ((map.map) show $ _cellData input)
  hPutStrLn h $ ", \"merklePaths\": " 
  hPrintListOfLists h ((map.map) show $ _merklePaths input)
  hPutStrLn h $ "}"
  hClose h

--------------------------------------------------------------------------------

trueFalses :: [Bool]
trueFalses = True : repeat False

indent :: Int -> String
indent k = replicate k ' '

hPrintList' :: Show a => Handle -> (Bool -> String) -> [a] -> IO ()
hPrintList' h indentation xs = do
  forM_ (zip trueFalses xs) $ \(b,x) -> do
    hPutStrLn h (indentation b ++ (if b then "[ " else ", ") ++ show x)
  hPutStrLn h (indentation False ++ "]")

hPrintList :: Show a => Handle -> Int -> [a] -> IO ()
hPrintList h indentBy xs = hPrintList' h (\_ -> indent indentBy) $ xs

hPrintListOfLists :: Show a => Handle -> [[a]] -> IO ()
hPrintListOfLists h xss = 
  do
    forM_ (zip trueFalses xss) $ \(b,xs) -> hPrintList' h (myIndentation b) xs
    hPutStrLn h ("    ]")
  where
    myIndentation True  True  = "    [ "
    myIndentation False True  = "    , "
    myIndentation _     False = "      "

--------------------------------------------------------------------------------
