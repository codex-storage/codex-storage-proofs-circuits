
import sugar
#import std/bitops
#import std/sequtils

import goldilocks_hash/types
#import goldilocks_hash/poseidon2/sponge
#import goldilocks_hash/monolith/sponge

import ../types
import ../types/goldilocks
import ../merkle/goldilocks/poseidon2
import ../merkle/goldilocks/monolith
import ../misc

#-------------------------------------------------------------------------------

func cellIndex*(hashcfg: HashConfig, entropy: Entropy, slotRoot: Digest, numberOfCells: int, counter: int): int =
  assert( hashcfg.field == Goldilocks )

  let log2 = ceilingLog2(numberOfCells)
  assert( 1 shl log2 == numberOfCells , "for this version, `numberOfCells` is assumed to be a power of two") 

  let inputD : seq[Digest] = @[ entropy, slotRoot, intToDigest( counter ) ]
  let input  : seq[F]      = digestSeqToFeltSeq(inputD)

  var hash : Digest
  case hashcfg.hashFun:
    of Poseidon2:  hash = digestFeltsPoseidon2( input )
    of Monolith:   hash = digestFeltsMonolith(  input )

  let hash4  : F4 = fromDigest(hash)
  let hash0  : F  = hash4[0]

  return int(extractLowBits(hash0,log2))

func cellIndices*(hashcfg: HashConfig, entropy: Entropy, slotRoot: Digest, numberOfCells: int, nSamples: int): seq[int] =
  return collect( newSeq, (for i in 1..nSamples: cellIndex(hashcfg, entropy, slotRoot, numberOfCells, i) ))

#-------------------------------------------------------------------------------
