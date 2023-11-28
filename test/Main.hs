
-- | Testing some sub-circuits
--

module Main where

--------------------------------------------------------------------------------

import R1CS.Misc ( Verbosity(..) )

import qualified R1CS.Test.Spec as Spec

import qualified Circuit.BinaryLTE     as BinaryLTE
import qualified Circuit.BinaryGTE     as BinaryGTE
import qualified Circuit.BinaryCompare as BinaryCmp
import qualified Circuit.ExtractBits   as ExtractBits
import qualified Circuit.Log2          as Log2
import qualified Circuit.CeilingLog2   as CeilingLog2

--------------------------------------------------------------------------------

testSimple :: IO ()
testSimple = testSimple' Silent

testSimple' :: Verbosity -> IO ()
testSimple' verbosity = do

  let runSpec     what = Spec.testSemantics     what verbosity
  let runSpecMany what = Spec.testSemanticsMany what verbosity

  runSpecMany CeilingLog2.specs
  runSpecMany Log2.specs

  runSpecMany BinaryCmp.specs
  runSpecMany BinaryLTE.specs
  runSpecMany BinaryGTE.specs

  runSpecMany ExtractBits.specs

--------------------------------------------------------------------------------

main = do
  testSimple' Info --Silent -- Verbose  -- Silent

