

import sugar
import std/sequtils

import goldilocks_hash/types
#import goldilocks_hash/poseidon2/sponge
#import goldilocks_hash/monolith/sponge

import ../types
import ../types/goldilocks
#import ../merkle
import ../merkle/goldilocks/poseidon2
import ../merkle/goldilocks/monolith

#-------------------------------------------------------------------------------

func merkleTree*( hashcfg: HashConfig, what: openarray[Digest] ): MerkleTree[Digest] =
  assert( hashcfg.field == Goldilocks )
  case hashcfg.hashFun:
    of Poseidon2: return poseidon2.merkleTreeGoldilocksPoseidon2( what )
    of Monolith:  return  monolith.merkleTreeGoldilocksMonolith( what )
 
#-------------------------------------------------------------------------------

func hashCellOpen( hashcfg: HashConfig, globcfg: GlobalConfig, cellData: openArray[byte] ): Digest = 
  assert( hashcfg.field == Goldilocks )
  assert( cellData.len == globcfg.cellSize , ("cells are expected to be exactly " & $globcfg.cellSize & " bytes") )
  case hashcfg.hashFun:
    of Poseidon2: return digestBytesPoseidon2( cellData )
    of Monolith:  return digestBytesMonolith(  cellData )

func hashCell*(hashcfg: HashConfig,  globcfg: GlobalConfig, cellData: Cell): Digest =  
  hashCellOpen(hashcfg, globcfg, cellData)

#-------------------------------------------------------------------------------

func splitBlockIntoCells( globcfg: GlobalConfig, blockData: openArray[byte] ): seq[Cell] = 
  assert( blockData.len == globcfg.blockSize , ("network blocks are expected to be exactly" & $globcfg.blockSize & " bytes" ) )

  var cells : seq[seq[byte]] = newSeq[seq[byte]]( cellsPerBlock(globcfg) )

  let start = low(blockData)
  var leaves : seq[Digest] = newSeq[Digest]( cellsPerBlock(globcfg) )
  for i in 0..<cellsPerBlock(globcfg):
    let a = start +  i    * globcfg.cellSize
    let b = start + (i+1) * globcfg.cellSize
    cells[i] = blockData[a..<b].toSeq()
   
  return cells

# returns the special hash of a network block (this is a Merkle root built on the
# top of the hashes of the 32 cells inside the block)
func hashNetworkBlockOpen( hashcfg: HashConfig,  globcfg: GlobalConfig, blockData: openArray[byte] ): Digest = 
  let cells  = splitBlockIntoCells(globcfg, blockData)
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock(globcfg): hashCell(hashcfg, globcfg, cells[i]) ))
  case hashcfg.hashFun:
    of Poseidon2: return merkleDigestPoseidon2(leaves)
    of Monolith:  return merkleDigestMonolith( leaves)

func hashNetworkBlock*( hashcfg: HashConfig, globcfg: GlobalConfig, blockData: Block): Digest = 
  hashNetworkBlockOpen(hashcfg, globcfg, blockData)

#-------------------------------------------------------------------------------

# returns the mini Merkle tree built on the 32 cells inside a network block
func networkBlockTreeOpen( hashcfg: HashConfig, globcfg: GlobalConfig, blockData: openArray[byte] ): MerkleTree[Digest] =
  assert( hashcfg.field == Goldilocks )
  let cells  = splitBlockIntoCells( globcfg, blockData )
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock(globcfg): hashCell( hashcfg, globcfg, cells[i]) ))
  return merkleTree(hashcfg, leaves)

func networkBlockTree*( hashcfg: HashConfig, globcfg: GlobalConfig, blockData: Block): MerkleTree[Digest] = 
  networkBlockTreeOpen(hashcfg, globcfg, blockData)

#-------------------------------------------------------------------------------
