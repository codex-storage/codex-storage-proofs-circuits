
module Main where

--------------------------------------------------------------------------------

import Slot ( SlotIdx(..) , DataSource(..) , Seed(..) )
import DataSet
import Sampling 

--------------------------------------------------------------------------------

smallDataSetCfg :: DataSetCfg
smallDataSetCfg = MkDataSetCfg 
  { _maxDepth      = 16
  , _maxLog2NSlots = 5
  , _nSlots    = 5
  , _cellSize  = 128
  , _blockSize = 1024
  , _nCells    = 64
  , _nSamples  = 10
  , _dataSrc   = FakeData (Seed 12345)
  }

bigDataSetCfg :: DataSetCfg
bigDataSetCfg = MkDataSetCfg 
  { _maxDepth      = 32
  , _maxLog2NSlots = 8
  , _nSlots    = 13
  , _cellSize  = 2048
  , _blockSize = 65536
  , _nCells    = 512
  , _nSamples  = 5
  , _dataSrc   = FakeData (Seed 666)
  }

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let dsetCfg = smallDataSetCfg
  let slotIdx = SlotIdx 3
  let entropy = 1234567 :: Entropy
  circomMainComponent dsetCfg                 "./json/slot_main.circom" 
  samplingTest        dsetCfg slotIdx entropy "./json/input_example.json"
