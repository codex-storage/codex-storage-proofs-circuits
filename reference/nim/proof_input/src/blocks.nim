
import sugar
import std/sequtils

#import poseidon2/types
import poseidon2/io
import poseidon2/sponge
import poseidon2/merkle

import types
import merkle

#-------------------------------------------------------------------------------

func hashCellOpen( globcfg: GlobalConfig, cellData: openArray[byte] ): Hash = 
  assert( cellData.len == globcfg.cellSize , ("cells are expected to be exactly " & $globcfg.cellSize & " bytes") )
  return Sponge.digest( cellData, rate=2 )

func hashCell*( globcfg: GlobalConfig, cellData: Cell): Hash =  hashCellOpen(globcfg, cellData)

#-------------------------------------------------------------------------------

func splitBlockIntoCells( globcfg: GlobalConfig, blockData: openArray[byte] ): seq[Cell] = 
  assert( blockData.len == globcfg.blockSize , ("network blocks are expected to be exactly" & $globcfg.blockSize & " bytes" ) )

  var cells : seq[seq[byte]] = newSeq[seq[byte]]( cellsPerBlock(globcfg) )

  let start = low(blockData)
  var leaves : seq[Hash] = newSeq[Hash]( cellsPerBlock(globcfg) )
  for i in 0..<cellsPerBlock(globcfg):
    let a = start +  i    * globcfg.cellSize
    let b = start + (i+1) * globcfg.cellSize
    cells[i] = blockData[a..<b].toSeq()
   
  return cells

# returns the special hash of a network block (this is a Merkle root built on the
# top of the hashes of the 32 cells inside the block)
func hashNetworkBlockOpen( globcfg: GlobalConfig, blockData: openArray[byte] ): Hash = 
  let cells  = splitBlockIntoCells(globcfg, blockData)
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock(globcfg): hashCell(globcfg, cells[i]) ))
  return merkleRoot(leaves)

func hashNetworkBlock*(globcfg: GlobalConfig, blockData: Block): Hash = 
  hashNetworkBlockOpen(globcfg, blockData)

#-------------------------------------------------------------------------------

# returns the mini Merkle tree built on the 32 cells inside a network block
func networkBlockTreeOpen( globcfg: GlobalConfig, blockData: openArray[byte] ): MerkleTree =
  let cells  = splitBlockIntoCells( globcfg, blockData)
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock(globcfg): hashCell( globcfg, cells[i]) ))
  return merkleTree(leaves)

func networkBlockTree*( globcfg: GlobalConfig, blockData: Block): MerkleTree = 
  networkBlockTreeOpen(globcfg, blockData)

#-------------------------------------------------------------------------------
