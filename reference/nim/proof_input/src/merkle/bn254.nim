
import std/sequtils

import
  constantine/math/arithmetic,
  constantine/math/io/io_fields

import poseidon2/types
import poseidon2/compress
import poseidon2/merkle
import poseidon2/io

import ../types
import ../types/bn254

#-------------------------------------------------------------------------------

func compressWithkey*(key: int, x, y: F): F = compress(x,y, key=toF(key))

func merkleDigestBN254*(xs: openArray[F]): F = Merkle.digest(xs)

#-------------------------------------------------------------------------------

const KeyNone              = F.fromHex("0x0")
const KeyBottomLayer       = F.fromHex("0x1")
const KeyOdd               = F.fromHex("0x2")
const KeyOddAndBottomLayer = F.fromhex("0x3")

func merkleTreeWorker(xs: openArray[F], isBottomLayer: static bool) : seq[seq[F]] =
  let a = low(xs)
  let b = high(xs)
  let m = b-a+1

  when not isBottomLayer:
    if m==1:
      return @[ xs.toSeq() ]

  let halfn  : int  = m div 2
  let n      : int  = 2*halfn
  let isOdd : bool = (n != m)

  var ys : seq[F]
  if not isOdd:
    ys = newSeq[F](halfn)
  else:
    ys = newSeq[F](halfn+1)

  for i in 0..<halfn:
    const key = when isBottomLayer: KeyBottomLayer else: KeyNone
    ys[i] = compress( xs[a+2*i], xs[a+2*i+1], key = key )
  if isOdd:
    const key = when isBottomLayer: KeyOddAndBottomLayer else: KeyOdd
    ys[halfn] = compress( xs[n], zero, key = key )

  var ls : seq[seq[F]] 
  ls = @[ xs.toSeq() ] 
  ls = ls & merkleTreeWorker(ys, isBottomLayer = false)
  return ls

#-------------------------------------------------------------------------------

func merkleTreeBN254*(xs: openArray[F]) : MerkleTree[F] =
  return MerkleTree[F](layers: merkleTreeWorker(xs, isBottomLayer = true))

#-------------------------------------------------------------------------------
