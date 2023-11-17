
import std/bitops
import std/sequtils

import constantine/math/arithmetic
import constantine/math/io/io_fields

import poseidon2/types
import poseidon2/merkle
import poseidon2/compress

import types


#-------------------------------------------------------------------------------

func treeDepth*(tree: MerkleTree): int = 
  return tree.layers.len - 1

func treeNumberOfLeaves*(tree: MerkleTree): int = 
  return tree.layers[0].len

func treeRoot*(tree: MerkleTree): Hash = 
  let last = tree.layers[tree.layers.len-1]
  assert( last.len == 1 )
  return last[0] 

#-------------------------------------------------------------------------------

func merkleProof*(tree: MerkleTree, index: int): MerkleProof = 
  let depth   = treeDepth(tree)
  let nleaves = treeNumberOfLeaves(tree)

  assert( index >= 0 and index < nleaves )

  var path : seq[Hash] = newSeq[Hash](depth)
  var k = index
  var m = nleaves
  for i in 0..<depth:
    let j = k xor 1
    path[i] = if (j < m): tree.layers[i][j] else: zero
    k =  k    shr 1
    m = (m+1) shr 1

  return MerkleProof( leafIndex:      index
                    , leafValue:      tree.layers[0][index]
                    , merklePath:     path
                    , numberOfLeaves: nleaves
                    )

#-------------------------------------------------------------------------------

func compressWithKey(key: int, x: F, y: F): F = 
  compress(x,y, key=toF(key))

func reconstructRoot*(proof: MerkleProof): Hash = 
  var m : int  = proof.numberOfLeaves
  var j : int  = proof.leafIndex
  var h : Hash = proof.leafValue
  var bottomFlag : int = 1
  for p in proof.merklePath:
    let oddIndex : bool = (bitand(j,1) != 0)
    if oddIndex:
      # the index of the child is odd, so the node tiself can't be odd (a bit counterintuitive, yeah :)
      let key = bottomFlag
      h = compressWithKey( key , p , h )
    else:
      if j==m-1:
        # single child => odd node
        let key = bottomFlag + 2
        h = compressWithKey( key , h , p )
      else:
        # even node
        let key = bottomFlag
        h = compressWithKey( key , h , p )
    bottomFlag = 0
    j =  j    shr 1
    m = (m+1) shr 1
  return h

func checkMerkleProof*(root: Root, proof: MerkleProof): bool = 
  return bool(root == reconstructRoot(proof))

#-------------------------------------------------------------------------------
# TODO: maybe move this (and the rest?) into poseidon2-nim

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

func merkleTree*(xs: openArray[F]) : MerkleTree =
  return MerkleTree(layers: merkleTreeWorker(xs, isBottomLayer = true))

#-------------------------------------------------------------------------------

#
# given a top tree, with small, fixed size (!) trees grafted to the leaves, 
# we can compose proofs (though for checking these proofs we need to remember 
# this and use a specific custom convention, because we mark the bottom layers)
#
func mergeMerkleProofs*(bottomProof, topProof: MerkleProof): MerkleProof =

  let botRoot = reconstructRoot( bottomProof )
  assert( bool(botRoot == topProof.leafValue) )
 
  let idx  = topProof.leafIndex * bottomProof.numberOfLeaves + bottomProof.leafIndex
  let val  = bottomProof.leafValue
  let nlvs = bottomProof.numberOfLeaves * topProof.numberOfLeaves
  let path = bottomProof.merklePath     & topProof.merklePath

  return MerkleProof( leafIndex:      idx
                    , leafValue:      val
                    , merklePath:     path
                    , numberOfLeaves: nlvs
                    )

#-------------------------------------------------------------------------------
