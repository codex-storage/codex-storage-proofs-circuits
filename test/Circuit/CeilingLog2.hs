
module Circuit.CeilingLog2 where

--------------------------------------------------------------------------------

import Data.Bits

import CircuitCommon

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "log2.circom"

-- `n` = maximum number of bits
type GP = Int

mainComponent :: GP -> MainComponent
mainComponent n = MainComponent 
  { _templateName   = "CeilingLog2"
  , _templateParams = [n]
  , _publicInputs   = ["inp"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = Integer
type Output   = (Int,[Bool],[Bool])

semantics :: GP -> TestCase -> Expected Output
semantics n a  
  | a > 0 && k >= 0 && k <= n  = Expecting (k,bits,mask) 
  | otherwise                  = ShouldFail
  where
    k = ceilingLog2 a
    mask = [ i < k | i<-[0..n] ]
    bits = [ testBit (a-1) i | i<-[0..n-1] ]

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Integer -> Int
ceilingLog2 0 = 0
ceilingLog2 n = 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

testCases :: GP -> [TestCase]
testCases n = [0..2^(n+1)+3] -- [-3..2^n+3]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: GP -> TestCase -> Inputs Name Integer
inputs n a = Inputs $ toMapping "inp" a

outputs :: Output -> Outputs Name Integer
outputs (y,bits,mask) = Outputs $  toMapping "out"  y
                                <> toMapping "bits" bits
                                <> toMapping "mask" mask

--------------------------------------------------------------------------------

spec :: GP -> TestSpec TestCase Output
spec n = TestSpec circomFile (mainComponent n) (inputs n) outputs (semantics n) (testCases n)

specs :: [ ( GP, TestSpec TestCase Output) ]
specs = [ (n, spec n) | n <- [1..7] ]

--------------------------------------------------------------------------------

