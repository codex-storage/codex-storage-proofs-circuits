
#
# generate the input data for the proof
# see `json.nim` to export it in Snarkjs-compatible format
#

import sugar
import std/sequtils

import ../blocks/goldilocks
import ../slot
import ../dataset
import ../sample/goldilocks
import ../merkle
import ../merkle/goldilocks/poseidon2
import ../merkle/goldilocks/monolith
import ../types
import ../types/goldilocks

#-------------------------------------------------------------------------------

proc buildSlotTreeFull( hashcfg: HashConfig, globcfg: GlobalConfig, slotCfg: SlotConfig ): (seq[MerkleTree[Digest]], MerkleTree[Digest]) =

  let ncells  = slotCfg.nCells
  let nblocks = ncells div cellsPerBlock(globcfg)
  assert( nblocks * cellsPerBlock(globcfg) == ncells )
  let blocks      : seq[Block]              = collect( newSeq, (for i in 0..<nblocks: slotLoadBlockData(globcfg, slotCfg, i) ))
  let miniTrees   : seq[MerkleTree[Digest]] = collect( newSeq, (for blk in blocks:     networkBlockTree(hashcfg, globcfg, blk) ))
  let blockHashes : seq[Root]               = map( miniTrees , treeRoot         )
  let bigTree  = merkleTree( hashcfg, blockHashes )
  return (miniTrees, bigTree)

proc buildSlotTree( hashcfg: HashConfig, globcfg: GlobalConfig, slotCfg: SlotConfig ): MerkleTree[Digest] = 
  return buildSlotTreeFull(hashcfg, globcfg, slotCfg)[1]

proc generateProofInput*( hashCfg: HashConfig, globCfg: GlobalConfig, dsetCfg: DataSetConfig, slotIdx: SlotIdx, entropy: Entropy ): SlotProofInput[Digest] =
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

  let dsetTree  = merkleTree( hashcfg, slotRoots )
  let dsetRoot  = treeRoot( dsetTree )
  let slotProof = merkleProof( dsetTree , slotIdx )

  let indices  = cellIndices(hashCfg, entropy, ourSlotRoot, ncells, dsetCfg.nSamples)

  var inputs : seq[CellProofInput[Digest]]
  for cellIdx in indices:
    let (miniTrees, bigTree) = buildSlotTreeFull( hashCfg, globCfg, ourSlotCfg )
    let blockIdx  = cellIdx div cellsPerBlock(globcfg)
    let blockTree = miniTrees[ blockIdx ]
    let cellData  = slotLoadCellData( globCfg, ourSlotCfg, cellIdx )
    let botProof  = merkleProof( blockTree , cellIdx mod cellsPerBlock(globcfg) )
    let topProof  = merkleProof( bigTree   , blockIdx )
    
    var prf : MerkleProof[Digest]
    case hashCfg.hashFun:
      of Poseidon2: prf = padMerkleProof( mergeMerkleProofs( poseidon2.compressWithKey, botProof, topProof ), globCfg.maxDepth )
      of Monolith:  prf = padMerkleProof( mergeMerkleProofs( monolith.compressWithKey , botProof, topProof ), globCfg.maxDepth )
    
    inputs.add( CellProofInput[Digest](cellData: cellData, merkleProof: prf) )

  return SlotProofInput[Digest]( dataSetRoot: dsetRoot
                               , entropy:     entropy
                               , nCells:      ncells
                               , nSlots:      nslots
                               , slotIndex:   slotIdx
                               , slotRoot:    ourSlotRoot 
                               , slotProof:   padMerkleProof( slotProof, globCfg.maxLog2NSlots )
                               , proofInputs: inputs
                               )

#---------------------------------------

proc generateProofInputGoldilocks*( hashCfg: HashConfig, globCfg: GlobalConfig, dsetCfg: DataSetConfig, slotIdx: SlotIdx, entropy: Entropy ): SlotProofInput[Digest] =
  generateProofInput( hashCfg, globCfg, dsetCfg, slotIdx, entropy)

#-------------------------------------------------------------------------------
