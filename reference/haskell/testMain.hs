
module Main where

import Slot
import Sampling 

main :: IO ()
main = do
  let slotCfg = exSlotCfg
  circomMainComponentV1 "slot_main.circom" slotCfg
  samplingTest "input_example.json"