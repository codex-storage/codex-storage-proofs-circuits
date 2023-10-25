
module CircuitCommon
  ( module R1CS
  , module System.FilePath
  , circuitSourceDir
  , toBitsLE , toBitsLE'
  )
  where 

--------------------------------------------------------------------------------

import Data.Bits
import System.FilePath
import R1CS

--------------------------------------------------------------------------------

circuitSourceDir :: FilePath
circuitSourceDir = "../circuit"

--------------------------------------------------------------------------------

toBitsLE :: Integer -> [Int]
toBitsLE = go where
  go 0 = []
  go n = fromInteger (n .&. 1) : go (shiftR n 1)

toBitsLE' :: Int -> Integer -> [Int]
toBitsLE' n what = take n (toBitsLE what ++ repeat 0)

--------------------------------------------------------------------------------
