
import std/bitops

import types

#-------------------------------------------------------------------------------

func treeDepth*[H](tree: MerkleTree[H]): int = 
  return tree.layers.len - 1

func treeNumberOfLeaves*[H](tree: MerkleTree[H]): int = 
  return tree.layers[0].len

func treeRoot*[H](tree: MerkleTree[H]): H = 
  let last = tree.layers[tree.layers.len-1]
  assert( last.len == 1 )
  return last[0] 

#-------------------------------------------------------------------------------

func merkleProof*[H](tree: MerkleTree[H], index: int): MerkleProof[H] = 
  let depth   = treeDepth(tree)
  let nleaves = treeNumberOfLeaves(tree)

  var zero : H      # hackety hack, it should be initialized with zeros

  assert( index >= 0 and index < nleaves )

  var path : seq[H] = newSeq[H](depth)
  var k = index
  var m = nleaves
  for i in 0..<depth:
    let j = k xor 1
    path[i] = if (j < m): tree.layers[i][j] else: zero
    k =  k    shr 1
    m = (m+1) shr 1

  return MerkleProof[H]( leafIndex:      index
                       , leafValue:      tree.layers[0][index]
                       , merklePath:     path
                       , numberOfLeaves: nleaves
                       )

#-------------------------------------------------------------------------------

type CompressWithKey[H] = proc (key: int, x, y: H): H {.closure.}

# func compressWithKey(key: int, x: F, y: F): F = 
#   compress(x,y, key=toF(key))

func reconstructRoot*[H](compressWithKey: CompressWithKey[H], proof: MerkleProof[H]): H = 
  var m : int = proof.numberOfLeaves
  var j : int = proof.leafIndex
  var h : H   = proof.leafValue
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

func checkMerkleProof*[H](compressWithKey: CompressWithKey[H], root: H, proof: MerkleProof[H]): bool = 
  return bool( root == reconstructRoot[H](compressWithKey, proof) )

#-------------------------------------------------------------------------------

#
# given a top tree, with small, fixed size (!) trees grafted to the leaves, 
# we can compose proofs (though for checking these proofs we need to remember 
# this and use a specific custom convention, because we mark the bottom layers)
#
func mergeMerkleProofs*[H](compressWithKey: CompressWithKey[H], bottomProof, topProof: MerkleProof[H]): MerkleProof[H] =

  let botRoot = reconstructRoot[H]( compressWithKey, bottomProof )
  assert( bool(botRoot == topProof.leafValue) )
 
  let idx  = topProof.leafIndex * bottomProof.numberOfLeaves + bottomProof.leafIndex
  let val  = bottomProof.leafValue
  let nlvs = bottomProof.numberOfLeaves * topProof.numberOfLeaves
  let path = bottomProof.merklePath     & topProof.merklePath

  return MerkleProof[H]( leafIndex:      idx
                       , leafValue:      val
                       , merklePath:     path
                       , numberOfLeaves: nlvs
                       )

#-------------------------------------------------------------------------------
