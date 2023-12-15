
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

proc buildSlotTreeFull( globcfg: GlobalConfig, slotCfg: SlotConfig ): (seq[MerkleTree], MerkleTree) =
  let ncells  = slotCfg.nCells
  let nblocks = ncells div cellsPerBlock(globcfg)
  assert( nblocks * cellsPerBlock(globcfg) == ncells )
  let blocks      : seq[Block]      = collect( newSeq, (for i in 0..<nblocks: slotLoadBlockData(globcfg, slotCfg, i) ))
  let miniTrees   : seq[MerkleTree] = collect( newSeq, (for blk in blocks:     networkBlockTree(globcfg, blk) ))
  let blockHashes : seq[Root]       = map( miniTrees , treeRoot         )
  let bigTree  = merkleTree( blockHashes )
  return (miniTrees, bigTree)

proc buildSlotTree( globcfg: GlobalConfig, slotCfg: SlotConfig ): MerkleTree = 
  return buildSlotTreeFull(globcfg, slotCfg)[1]

proc generateProofInput*( globCfg: GlobalConfig, dsetCfg: DataSetConfig, slotIdx: SlotIdx, entropy: Entropy ): SlotProofInput =
  let nslots  = dsetCfg.nSlots
  let ncells  = dsetCfg.nCells
  let nblocks = ncells div cellsPerBlock(globCfg)
  assert( nblocks * cellsPerBlock(globcfg) == ncells )

  let slotCfgs  = collect( newSeq , (for i in 0..<nslots: slotCfgFromDataSetCfg(dsetcfg, i) ))
  let slotTrees = collect( newSeq , (for scfg in slotcfgs: buildSlotTree(globCfg, scfg) ))
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
    let (miniTrees, bigTree) = buildSlotTreeFull( globCfg, ourSlotCfg )
    let blockIdx  = cellIdx div cellsPerBlock(globcfg)
    let blockTree = miniTrees[ blockIdx ]
    let cellData  = slotLoadCellData( globCfg, ourSlotCfg, cellIdx )
    let botProof  = merkleProof( blockTree , cellIdx mod cellsPerBlock(globcfg) )
    let topProof  = merkleProof( bigTree   , blockIdx )
    let prf       = padMerkleProof( mergeMerkleProofs( botProof, topProof ), globCfg.maxDepth )
    inputs.add( CellProofInput(cellData: cellData, merkleProof: prf) )

  return SlotProofInput( dataSetRoot: dsetRoot
                       , entropy:     entropy
                       , nCells:      ncells
                       , nSlots:      nslots
                       , slotIndex:   slotIdx
                       , slotRoot:    ourSlotRoot 
                       , slotProof:   padMerkleProof( slotProof, globCfg.maxLog2NSlots )
                       , proofInputs: inputs
                       )

#-------------------------------------------------------------------------------
