
import sugar
import std/bitops

import constantine/math/arithmetic

import poseidon2/types
import poseidon2/io
import poseidon2/sponge

import types
import misc

#-------------------------------------------------------------------------------

func extractLowBits[n: static int]( A: BigInt[n], k: int): uint64 = 
  assert( k>0 and k<=64 )
  var r : uint64 = 0
  for i in 0..<k:
    let b = bit[n](A, i)     # NOTE: the docunmentation seems to lie about the conventions here....
    let y = uint64(b)
    if (y != 0):
      r = bitor( r, 1'u64 shl i )
  return r

func extractLowBits(fld: F, k: int): uint64 = 
  let A : BigInt[254] = fld.toBig()
  return extractLowBits(A, k);

#-------------------------------------------------------------------------------

func cellIndex*(entropy: Entropy, slotRoot: Root, numberOfCells: int, counter: int): int =
  let log2 = ceilingLog2(numberOfCells)
  assert( 1 shl log2 == numberOfCells , "for this version, `numberOfCells` is assumed to be a power of two") 

  let input : seq[F] = @[ entropy, slotRoot, toF(counter) ]
  let H : Hash        = Sponge.digest( input, rate = 2 )
  return int(extractLowBits(H,log2))

func cellIndices*(entropy: Entropy, slotRoot: Root, numberOfCells: int, nSamples: int): seq[int] =
  return collect( newSeq, (for i in 1..nSamples: cellIndex(entropy, slotRoot, numberOfCells, i) ))

#-------------------------------------------------------------------------------
