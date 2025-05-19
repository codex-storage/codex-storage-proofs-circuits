
-- | Testing some sub-circuits
--

module Main where

--------------------------------------------------------------------------------

import R1CS

import qualified R1CS.Test.Spec as Spec

import qualified Circuit.BinaryLTE     as BinaryLTE
import qualified Circuit.BinaryGTE     as BinaryGTE
import qualified Circuit.BinaryCompare as BinaryCmp
import qualified Circuit.ExtractBits   as ExtractBits
import qualified Circuit.Log2          as Log2
import qualified Circuit.CeilingLog2   as CeilingLog2

--------------------------------------------------------------------------------

testSimple :: FieldChoice -> IO ()
testSimple field = testSimple' field Silent

testSimple' :: FieldChoice -> Verbosity -> IO ()
testSimple' field verbosity = runWithField field $ \pxy -> do

  let runSpec     what = Spec.testSemantics     pxy what verbosity
  let runSpecMany what = Spec.testSemanticsMany pxy what verbosity

  runSpecMany CeilingLog2.specs
  runSpecMany Log2.specs

  runSpecMany BinaryCmp.specs
  runSpecMany BinaryLTE.specs
  runSpecMany BinaryGTE.specs

  -- runSpecMany ExtractBits.specs      -- this test doesn't work currently? 

--------------------------------------------------------------------------------

main = do
  testSimple' Field20 Silent   -- Info

