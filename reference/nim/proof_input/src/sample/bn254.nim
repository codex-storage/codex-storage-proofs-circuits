
import sugar
#import std/bitops

# import constantine/math/arithmetic
import poseidon2/types
import poseidon2/sponge
import poseidon2/io

import ../types
import ../types/bn254
import ../misc

#-------------------------------------------------------------------------------

func cellIndex*(hashcfg: HashConfig, entropy: Entropy, slotRoot: Root, numberOfCells: int, counter: int): int =
  assert( hashcfg.field == BN254 )

  let log2 = ceilingLog2(numberOfCells)
  assert( 1 shl log2 == numberOfCells , "for this version, `numberOfCells` is assumed to be a power of two") 

  let input : seq[F] = @[ entropy, slotRoot, toF(counter) ]
  let H : Hash        = Sponge.digest( input, rate = 2 )
  return int(extractLowBits(H,log2))

func cellIndices*(hashcfg: HashConfig, entropy: Entropy, slotRoot: Root, numberOfCells: int, nSamples: int): seq[int] =
  return collect( newSeq, (for i in 1..nSamples: cellIndex(hashcfg, entropy, slotRoot, numberOfCells, i) ))

#-------------------------------------------------------------------------------
