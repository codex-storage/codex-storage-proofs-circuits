
module Main where

import Slot
import Sampling 

smallSlotCfg :: SlotConfig
smallSlotCfg = MkSlotCfg
  { _cellSize  = 128
  , _blockSize = 4096
  , _nCells    = 256
  , _nSamples  = 5
  , _dataSrc   = FakeData 12345
  }

bigSlotCfg :: SlotConfig
bigSlotCfg = MkSlotCfg
  { _cellSize  = 2048
  , _blockSize = 65536
  , _nCells    = 512
  , _nSamples  = 5
  , _dataSrc   = FakeData 666
  }

main :: IO ()
main = do
  let slotCfg = smallSlotCfg
  let entropy = 1234567 :: Entropy
  circomMainComponentV1 slotCfg         "./json/slot_main.circom" 
  samplingTest          slotCfg entropy "./json/input_example.json"