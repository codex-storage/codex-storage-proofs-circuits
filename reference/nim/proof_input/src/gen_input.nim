
#
# generate the input data for the proof
# see `json.nim` to export it in Snarkjs-compatible format
#

import sugar
import std/sequtils

import blocks
import slot
import sample
import merkle
import types

#-------------------------------------------------------------------------------

proc generateProofInput*( cfg: SlotConfig, entropy: Entropy ): SlotProofInput =

  let ncells  = cfg.nCells
  let nblocks = ncells div cellsPerBlock

  assert( nblocks * cellsPerBlock == ncells )

  let blocks      : seq[Block]      = collect( newSeq, (for i in 0..<nblocks: loadBlockData(cfg, i) ))
  let miniTrees   : seq[MerkleTree] = map( blocks    , networkBlockTree )
  let blockHashes : seq[Root]       = map( miniTrees , treeRoot         )

  let bigTree  = merkleTree( blockHashes )
  let slotRoot = treeRoot( bigTree )

  let indices  = cellIndices(entropy, slotRoot, ncells, cfg.nSamples)

  var inputs : seq[CellProofInput]
  for cellIdx in indices:
    let blockIdx  = cellIdx div cellsPerBlock
    let blockTree = miniTrees[ blockIdx ]
    let cellData  = loadCellData(  cfg, cellIdx )
    let botProof  = merkleProof( blockTree , cellIdx mod cellsPerBlock )
    let topProof  = merkleProof( bigTree   , blockIdx )
    let prf  = mergeMerkleProofs( botProof, topProof )
    inputs.add( CellProofInput(cellData: cellData, merkleProof: prf) )

  return SlotProofInput( slotRoot:    slotRoot 
                       , entropy:     entropy
                       , nCells:      ncells
                       , proofInputs: inputs
                       )

#-------------------------------------------------------------------------------
