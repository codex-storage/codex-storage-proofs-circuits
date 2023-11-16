
module Main where

import Slot
import Sampling 

mySlotCfg :: SlotConfig
mySlotCfg = MkSlotCfg
  { _cellSize = 128
  , _nCells   = 1024
  , _nSamples = 10
  , _dataSrc  = FakeData 12345
  }

main :: IO ()
main = do
  let slotCfg = mySlotCfg
  circomMainComponentV1 slotCfg "slot_main.circom" 
  samplingTest          slotCfg "input_example.json"