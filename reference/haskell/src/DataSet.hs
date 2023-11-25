
{-# LANGUAGE StrictData #-}
module DataSet where

--------------------------------------------------------------------------------

import System.FilePath

import Slot hiding ( MkSlotCfg(..) )
import qualified Slot as Slot

--------------------------------------------------------------------------------

data DataSetCfg = MkDataSetCfg 
  { _nSlots    :: Int           -- ^ number of slots per dataset
  , _cellSize  :: Int
  , _blockSize :: Int
  , _nCells    :: Int
  , _nSamples  :: Int
  , _dataSrc   :: DataSource
  }
  deriving Show

fieldElemsPerCell :: DataSetCfg -> Int
fieldElemsPerCell cfg = (DataSet._cellSize cfg + 30) `div` 31

dataSetSlotCfg :: DataSetCfg -> SlotIdx -> SlotConfig
dataSetSlotCfg dsetCfg idx = Slot.MkSlotCfg
  { Slot._cellSize  = DataSet._cellSize  dsetCfg
  , Slot._blockSize = DataSet._blockSize dsetCfg
  , Slot._nCells    = DataSet._nCells    dsetCfg
  , Slot._nSamples  = DataSet._nSamples  dsetCfg
  , Slot._dataSrc   = parametricSlotDataSource (DataSet._dataSrc dsetCfg) idx
  }

--------------------------------------------------------------------------------

loadDataSetCell :: DataSetCfg -> SlotIdx -> CellIdx -> IO CellData
loadDataSetCell dsetCfg slotIdx@(SlotIdx idx) cellidx
  | idx < 0                 = error "loadDataSetCell: negative slot index"
  | idx >= _nSlots dsetCfg  = error "loadDataSetCell: slot index out of range"
  | otherwise               = loadCellData (dataSetSlotCfg dsetCfg slotIdx) cellidx
  

loadDataSetBlock :: DataSetCfg -> SlotIdx -> BlockIdx -> IO BlockData
loadDataSetBlock dsetCfg slotIdx@(SlotIdx idx) blockidx
  | idx < 0                 = error "loadDataSetBlock: negative slot index"
  | idx >= _nSlots dsetCfg  = error "loadDataSetBlock: slot index out of range"
  | otherwise               = loadBlockData (dataSetSlotCfg dsetCfg slotIdx) blockidx

--------------------------------------------------------------------------------

-- | Writes a @circom@ main component with the given parameters
--
-- > template SampleAndProve( nFieldElemsPerCell, nSamples ) { ... }
--
circomMainComponent :: DataSetCfg -> FilePath -> IO ()
circomMainComponent dsetCfg circomFile = do

  let params =          show (DataSet.fieldElemsPerCell dsetCfg)
             ++ ", " ++ show (DataSet._nSamples         dsetCfg)

  writeFile circomFile $ unlines
    [ "pragma circom 2.0.0;"
    , "include \"sample_cells.circom\";"
    , "component main {public [entropy,dataSetRoot,slotIndex]} = SampleAndProve(" ++ params ++ ");"
    ]

--------------------------------------------------------------------------------

parametricSlotSeed :: Seed -> SlotIdx -> Seed
parametricSlotSeed (Seed seed) (SlotIdx k) = Seed (seed + 72 + 1001*k)

-- | From @dir/dset.dat@ we make @dir/dset5.dat@ for the 5-th slot
parametricSlotFileName :: FilePath -> SlotIdx -> FilePath
parametricSlotFileName basefile (SlotIdx k) =
  (dropExtension basefile ++ show k) <.> (takeExtension basefile)

parametricSlotDataSource :: DataSource -> SlotIdx -> DataSource
parametricSlotDataSource src idx = case src of
  FakeData seed  -> FakeData (parametricSlotSeed     seed  idx)
  SlotFile fpath -> SlotFile (parametricSlotFileName fpath idx)

--------------------------------------------------------------------------------
