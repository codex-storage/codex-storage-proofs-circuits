
module CircuitCommon
  ( module R1CS
  , module System.FilePath
  , circuitRootDir
  , circuitLibSourceDir
  , toBitsLE , toBitsLE'
  )
  where 

--------------------------------------------------------------------------------

import Data.Bits
import System.FilePath
import R1CS

--------------------------------------------------------------------------------

circuitRootDir :: FilePath
circuitRootDir = "../circuit"

circuitLibSourceDir :: FilePath
circuitLibSourceDir = circuitRootDir </> "lib"

--------------------------------------------------------------------------------

toBitsLE :: Integer -> [Int]
toBitsLE = go where
  go 0 = []
  go n = fromInteger (n .&. 1) : go (shiftR n 1)

toBitsLE' :: Int -> Integer -> [Int]
toBitsLE' n what = take n (toBitsLE what ++ repeat 0)

--------------------------------------------------------------------------------
