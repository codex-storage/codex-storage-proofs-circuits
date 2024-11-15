
#
# generate the input data for the proof
# see `json.nim` to export it in Snarkjs-compatible format
#

import sugar
import std/sequtils

import ../blocks/bn254
import ../slot
import ../dataset
import ../sample/bn254
import ../merkle
import ../merkle/bn254
import ../types
import ../types/bn254

#-------------------------------------------------------------------------------

proc buildSlotTreeFull( hashcfg: HashConfig, globcfg: GlobalConfig, slotCfg: SlotConfig ): (seq[MerkleTree[Hash]], MerkleTree[Hash]) =

  let ncells  = slotCfg.nCells
  let nblocks = ncells div cellsPerBlock(globcfg)
  assert( nblocks * cellsPerBlock(globcfg) == ncells )
  let blocks      : seq[Block]            = collect( newSeq, (for i in 0..<nblocks: slotLoadBlockData(globcfg, slotCfg, i) ))
  let miniTrees   : seq[MerkleTree[Hash]] = collect( newSeq, (for blk in blocks:     networkBlockTree(hashcfg, globcfg, blk) ))
  let blockHashes : seq[Root]             = map( miniTrees , treeRoot         )
  let bigTree  = merkleTree( hashcfg, blockHashes )
  return (miniTrees, bigTree)

proc buildSlotTree( hashcfg: HashConfig, globcfg: GlobalConfig, slotCfg: SlotConfig ): MerkleTree[Hash] = 
  return buildSlotTreeFull(hashcfg, globcfg, slotCfg)[1]

proc generateProofInput*( hashCfg: HashConfig, globCfg: GlobalConfig, dsetCfg: DataSetConfig, slotIdx: SlotIdx, entropy: Entropy ): SlotProofInput[Hash] =
  let nslots  = dsetCfg.nSlots
  let ncells  = dsetCfg.nCells
  let nblocks = ncells div cellsPerBlock(globCfg)
  assert( nblocks * cellsPerBlock(globcfg) == ncells )

  let slotCfgs  = collect( newSeq , (for i in 0..<nslots: slotCfgFromDataSetCfg(dsetcfg, i) ))
  let slotTrees = collect( newSeq , (for scfg in slotcfgs: buildSlotTree(hashCfg, globCfg, scfg) ))
  let slotRoots = map( slotTrees, treeRoot )

  let ourSlotCfg  = slotCfgs[slotIdx] 
  let ourSlotRoot = slotRoots[slotIdx]
  let ourSlotTree = slotTrees[slotIdx]

  let dsetTree  : MerkleTree[Hash]  = merkleTree( hashcfg, slotRoots )
  let dsetRoot  : Hash              = treeRoot( dsetTree )
  let slotProof : MerkleProof[Hash] = merkleProof( dsetTree , slotIdx )

  let indices  = cellIndices(hashCfg, entropy, ourSlotRoot, ncells, dsetCfg.nSamples)

  var inputs : seq[CellProofInput[Hash]]
  for cellIdx in indices:
    let (miniTrees, bigTree) = buildSlotTreeFull( hashCfg, globCfg, ourSlotCfg )
    let blockIdx  = cellIdx div cellsPerBlock(globcfg)
    let blockTree = miniTrees[ blockIdx ]
    let cellData  = slotLoadCellData( globCfg, ourSlotCfg, cellIdx )
    let botProof  = merkleProof( blockTree , cellIdx mod cellsPerBlock(globcfg) )
    let topProof  = merkleProof( bigTree   , blockIdx )
    let prf       = padMerkleProof( mergeMerkleProofs( compressWithKey, botProof, topProof ), globCfg.maxDepth )
    inputs.add( CellProofInput[Hash](cellData: cellData, merkleProof: prf) )

  return SlotProofInput[Hash]( dataSetRoot: dsetRoot
                             , entropy:     entropy
                             , nCells:      ncells
                             , nSlots:      nslots
                             , slotIndex:   slotIdx
                             , slotRoot:    ourSlotRoot 
                             , slotProof:   padMerkleProof( slotProof, globCfg.maxLog2NSlots )
                             , proofInputs: inputs
                             )

#---------------------------------------

proc generateProofInputBN254*( hashCfg: HashConfig, globCfg: GlobalConfig, dsetCfg: DataSetConfig, slotIdx: SlotIdx, entropy: Entropy ): SlotProofInput[Hash] =
  generateProofInput( hashCfg, globCfg, dsetCfg, slotIdx, entropy)

#-------------------------------------------------------------------------------
