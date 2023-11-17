
import sugar
import std/sequtils

import constantine/math/arithmetic

import poseidon2/types
import poseidon2/merkle

import types
import blocks
import slot
import sample
import merkle
import gen_input
import json

#-------------------------------------------------------------------------------

proc testMerkleProofs*( input: seq[F] ) = 
  let tree = merkleTree(input)
  let root = merkleRoot(input)
  assert( bool(root == treeRoot(tree)) )

  let n  = input.len
  var ok = true
  var oks : seq[bool] = newSeq[bool]( n )
  for i in 0..<n: 
    let proof = merkleProof(tree, i)
    let b = checkMerkleProof(root, proof)
    oks[i] = b
    ok     = ok and b
  let prefix = ("testing Merkle proofs for an input of size " & $n & " ... ")
  if ok:
    echo (prefix & "OK.")
  else:
    echo (prefix & "FAILED!")
    echo oks

proc testAllMerkleProofs*( N: int ) = 
  for k in 1..N:
    let input = collect( newSeq , (for i in 1..k: toF(100+i) ))
    testMerkleProofs( input )

#-------------------------------------------------------------------------------

when isMainModule:
  # testAllMerkleProofs(20)

  let fakedata = DataSource(kind: FakeData, seed: 12345)
  let slotcfg  = SlotConfig( nCells: 128, nSamples: 3, dataSrc: fakedata)                      
  let entropy  = toF( 1234567 )
  let prfInput = generateProofInput(slotcfg, entropy)
  exportProofInput( "foo.json" , prfInput )

