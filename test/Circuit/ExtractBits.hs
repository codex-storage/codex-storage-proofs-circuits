
module Circuit.ExtractBits where

--------------------------------------------------------------------------------

import CircuitCommon

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "extract_bits.circom"

-- | extracting the lowest @n@-bit of the canonical representation of a field element
type GP = Int

mainComponent :: GP -> MainComponent
mainComponent n = MainComponent 
  { _templateName   = "ExtractLowerBits_testfield65537"
  , _templateParams = [n]
  , _publicInputs   = ["inp"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = Integer
type Output   = Int

semantics :: GP -> TestCase -> Expected Output
semantics n a = Expecting $ fromInteger (mod a (2^n))

testCases :: GP -> [TestCase]
testCases n = [0..20]
--testCases n =  [   a | a<-[0..2^(n+3)+7] ] 
--            ++ [ - a | a<-[1..2^(n+3)+7] ] 

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: GP -> TestCase -> Inputs Name Integer
inputs n a = Inputs $ toMapping "inp" a

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: GP -> TestSpec TestCase Output
spec n = TestSpec circomFile (mainComponent n) (inputs n) outputs (semantics n) (testCases n)

specs :: [ ( GP, TestSpec TestCase Output) ]
specs = [ (n, spec n) | n <- [2,3,4,5] ]

--------------------------------------------------------------------------------

