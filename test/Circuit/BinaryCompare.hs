
module Circuit.BinaryCompare where

--------------------------------------------------------------------------------

import CircuitCommon

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitLibSourceDir </> "binary_compare.circom"

-- | comparing @n@-bit integers
type GP = Int

mainComponent :: GP -> MainComponent
mainComponent n = MainComponent 
  { _templateName   = "BinaryCompare"
  , _templateParams = [n]
  , _publicInputs   = ["A","B"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = (Integer,Integer)
type Output   = Int

cmp :: Integer -> Integer -> Int
cmp a b = case compare a b of
  LT -> -1
  EQ ->  0 
  GT ->  1

semantics :: GP -> TestCase -> Expected Output
semantics n (a,b) = Expecting $ cmp a b

testCases :: GP -> [TestCase]
testCases n = [ (a,b) | a<-[0..2^n-1] , b<-[0..2^n-1] ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: GP -> TestCase -> Inputs Name Integer
inputs n (a,b) = Inputs $  toMapping "A" (toBitsLE' n a)
                        <> toMapping "B" (toBitsLE' n b)

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: GP -> TestSpec TestCase Output
spec n = TestSpec circomFile (mainComponent n) (inputs n) outputs (semantics n) (testCases n)

specs :: [ ( GP, TestSpec TestCase Output) ]
specs = [ (n, spec n) | n <- [4,5,7] ]

--------------------------------------------------------------------------------

