
#
# generate the input data for the proof
# see `json.nim` to export it in Snarkjs-compatible format
#

import sugar
import std/sequtils

import blocks
import slot
import dataset
import sample
import merkle
import types

#-------------------------------------------------------------------------------

proc buildSlotTreeFull( slotCfg: SlotConfig ): (seq[MerkleTree], MerkleTree) =
  let ncells  = slotCfg.nCells
  let nblocks = ncells div cellsPerBlock
  assert( nblocks * cellsPerBlock == ncells )
  let blocks      : seq[Block]      = collect( newSeq, (for i in 0..<nblocks: slotLoadBlockData(slotCfg, i) ))
  let miniTrees   : seq[MerkleTree] = map( blocks    , networkBlockTree )
  let blockHashes : seq[Root]       = map( miniTrees , treeRoot         )
  let bigTree  = merkleTree( blockHashes )
  return (miniTrees, bigTree)

proc buildSlotTree( slotCfg: SlotConfig ): MerkleTree = 
  return buildSlotTreeFull(slotCfg)[1]

proc generateProofInput*( dsetCfg: DataSetConfig, slotIdx: SlotIdx, entropy: Entropy ): SlotProofInput =
  let nslots  = dsetCfg.nSlots
  let ncells  = dsetCfg.nCells
  let nblocks = ncells div cellsPerBlock
  assert( nblocks * cellsPerBlock == ncells )

  let slotCfgs  = collect( newSeq , (for i in 0..<nslots: slotCfgFromDataSetCfg(dsetcfg, i) ))
  let slotTrees = map( slotCfgs, buildSlotTree )
  let slotRoots = map( slotTrees, treeRoot )

  let ourSlotCfg  = slotCfgs[slotIdx] 
  let ourSlotRoot = slotRoots[slotIdx]
  let ourSlotTree = slotTrees[slotIdx]

  let dsetTree  = merkleTree( slotRoots )
  let dsetRoot  = treeRoot( dsetTree )
  let slotProof = merkleProof( dsetTree , slotIdx )

  let indices  = cellIndices(entropy, ourSlotRoot, ncells, dsetCfg.nSamples)

  var inputs : seq[CellProofInput]
  for cellIdx in indices:
    let (miniTrees, bigTree) = buildSlotTreeFull( ourSlotCfg )
    let blockIdx  = cellIdx div cellsPerBlock
    let blockTree = miniTrees[ blockIdx ]
    let cellData  = slotLoadCellData( ourSlotCfg, cellIdx )
    let botProof  = merkleProof( blockTree , cellIdx mod cellsPerBlock )
    let topProof  = merkleProof( bigTree   , blockIdx )
    let prf  = mergeMerkleProofs( botProof, topProof )
    inputs.add( CellProofInput(cellData: cellData, merkleProof: prf) )

  return SlotProofInput( dataSetRoot: dsetRoot
                       , entropy:     entropy
                       , nCells:      ncells
                       , nSlots:      nslots
                       , slotIndex:   slotIdx
                       , slotRoot:    ourSlotRoot 
                       , slotProof:   slotProof
                       , proofInputs: inputs
                       )

#-------------------------------------------------------------------------------
