
import std/sequtils

import goldilocks_hash/types
import goldilocks_hash/poseidon2/compress
import goldilocks_hash/poseidon2/merkle
import goldilocks_hash/poseidon2/sponge

import ../../types
import ../../types/goldilocks

#-------------------------------------------------------------------------------

func compressWithkey*(key: int, x, y: Digest): Digest = compress(x,y, key=uint64(key))

func merkleDigestPoseidon2*(xs: openArray[Digest]): Digest = Merkle.digest(xs)

# TODO: move these somewhere else
func digestFeltsPoseidon2*(xs: openArray[F]   ): Digest = digestFeltsC( rate=8, xs )
func digestBytesPoseidon2*(xs: openArray[byte]): Digest = digestBytesC( rate=8, xs )

#-------------------------------------------------------------------------------

const KeyNone              : uint64 = 0x00
const KeyBottomLayer       : uint64 = 0x01
const KeyOdd               : uint64 = 0x02
const KeyOddAndBottomLayer : uint64 = 0x03

func merkleTreeWorker(xs: openArray[Digest], isBottomLayer: static bool) : seq[seq[Digest]] =
  let a = low(xs)
  let b = high(xs)
  let m = b-a+1

  when not isBottomLayer:
    if m==1:
      return @[ xs.toSeq() ]

  let halfn  : int  = m div 2
  let n      : int  = 2*halfn
  let isOdd : bool = (n != m)

  var ys : seq[Digest]
  if not isOdd:
    ys = newSeq[Digest](halfn)
  else:
    ys = newSeq[Digest](halfn+1)

  for i in 0..<halfn:
    const key = when isBottomLayer: KeyBottomLayer else: KeyNone
    ys[i] = compress( xs[a+2*i], xs[a+2*i+1], key = key )
  if isOdd:
    const key = when isBottomLayer: KeyOddAndBottomLayer else: KeyOdd
    ys[halfn] = compress( xs[n], zeroDigest, key = key )

  var ls : seq[seq[Digest]] 
  ls = @[ xs.toSeq() ] 
  ls = ls & merkleTreeWorker(ys, isBottomLayer = false)
  return ls

#-------------------------------------------------------------------------------

func merkleTreeGoldilocksPoseidon2*(xs: openArray[Digest]) : MerkleTree[Digest] =
  return MerkleTree[Digest](layers: merkleTreeWorker(xs, isBottomLayer = true))

#-------------------------------------------------------------------------------
