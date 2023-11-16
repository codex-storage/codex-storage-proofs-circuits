
module Slot where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

import Control.Monad
import System.IO

import Poseidon2

--------------------------------------------------------------------------------

type Seed    = Int
type CellIdx = Int
type Hash    = Fr

data DataSource
  = SlotFile FilePath
  | FakeData Seed
  deriving Show

data SlotConfig = MkSlotCfg
  { _cellSize :: Int           -- ^ cell size in bytes
  , _nCells   :: Int           -- ^ number of cells per slot (should be power of two)
  , _nSamples :: Int           -- ^ how many cells we sample
  , _dataSrc  :: DataSource    -- ^ slot data source
  }
  deriving Show

-- | Example slot configuration
exSlotCfg :: SlotConfig
exSlotCfg = MkSlotCfg
  { _cellSize = 256
  , _nCells   = 1024
  , _nSamples = 20
  , _dataSrc  = FakeData 12345
  }

fieldElemsPerCell :: SlotConfig -> Int
fieldElemsPerCell cfg = (_cellSize cfg + 30) `div` 31

--------------------------------------------------------------------------------

-- | Writes a @circom@ main component with the given parameters
--
-- > template SampleAndProveV1( nCells, nFieldElemsPerCell, nSamples ) { ... }
--
circomMainComponentV1 :: SlotConfig -> FilePath -> IO ()
circomMainComponentV1 slotCfg circomFile = do

  let params =          show (_nCells           slotCfg)
             ++ ", " ++ show (fieldElemsPerCell slotCfg)
             ++ ", " ++ show (_nSamples         slotCfg)

  writeFile circomFile $ unlines
    [ "pragma circom 2.0.0;"
    , "include \"sample_cells.circom\";"
    , "component main {public [entropy,slotRoot]} = SampleAndProveV1(" ++ params ++ ");"
    ]

--------------------------------------------------------------------------------
-- * load data

genFakeCell :: SlotConfig -> Seed -> CellIdx -> ByteString
genFakeCell cfg seed1 seed2 = B.pack list where
  list = go (_cellSize cfg) 1 
  go :: Int -> Int -> [Word8]
  go 0   _     = []
  go cnt state = fromIntegral state' : go (cnt-1) state' where
    state' = state*state + seed1*state + (seed2 + 17)

loadCellData :: SlotConfig -> CellIdx -> IO ByteString
loadCellData cfg idx = case _dataSrc cfg of
  FakeData seed  -> return $ genFakeCell cfg seed idx
  SlotFile fname -> do
    h <- openBinaryFile fname ReadMode
    hSeek h AbsoluteSeek (fromIntegral (_cellSize cfg) * fromIntegral idx)
    bs <- B.hGet h (_cellSize cfg)
    hClose h
    return bs

--------------------------------------------------------------------------------

calcSlotTree :: SlotConfig -> IO MerkleTree
calcSlotTree cfg = calcMerkleTree <$> calcCellHashes cfg

calcCellHashes :: SlotConfig -> IO [Hash]
calcCellHashes cfg = do
  forM [0..(_nCells cfg - 1)] $ \idx -> do
    cell <- loadCellData cfg idx 
    return (hashCell cell)

--------------------------------------------------------------------------------

-- | Split bytestring into smaller pieces
splitByteString :: Int -> ByteString -> [ByteString]
splitByteString k = go where
  go bs 
    | B.null bs  = []
    | otherwise  = B.take k bs : go (B.drop k bs)

-- | Chunk a ByteString into a sequence of field elements
cellDataToFieldElements :: ByteString -> [Fr]
cellDataToFieldElements rawdata = map chunkToField pieces where
  chunkSize = 31
  pieces = splitByteString chunkSize rawdata

-- | Hash a cell
hashCell :: ByteString -> Hash
hashCell rawdata = sponge2 (cellDataToFieldElements rawdata) 

chunkToField :: ByteString -> Fr
chunkToField chunk
  | B.length chunk <= 31  = fromInteger (chunkToIntegerLE chunk)
  | otherwise             = error "chunkToField: chunk is too big (expecting at most 31 bytes)"

-- | Interpret a ByteString as an integer (little-endian)
chunkToIntegerLE :: ByteString -> Integer
chunkToIntegerLE chunk = go (B.unpack chunk) where
  go []     = 0
  go (w:ws) = fromIntegral w + shiftL (go ws) 8

--------------------------------------------------------------------------------
