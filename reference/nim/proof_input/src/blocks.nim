
import sugar
import std/sequtils

#import poseidon2/types
import poseidon2/io
import poseidon2/sponge
import poseidon2/merkle

import types
import merkle

#-------------------------------------------------------------------------------

func hashCellOpen( cellData: openArray[byte] ): Hash = 
  assert( cellData.len == cellSize , "cells are expected to be exactly 2048 bytes" )
  return Sponge.digest( cellData, rate=2 )

func hashCell*(cellData: Cell): Hash =  hashCellOpen(cellData)

#-------------------------------------------------------------------------------

func splitBlockIntoCells( blockData: openArray[byte] ): seq[Cell] = 
  assert( blockData.len == blockSize , "network blocks are expected to be exactly 65536 bytes" )

  var cells : seq[seq[byte]] = newSeq[seq[byte]]( cellsPerBlock )

  let start = low(blockData)
  var leaves : seq[Hash] = newSeq[Hash]( cellsPerBlock )
  for i in 0..<cellsPerBlock:
    let a = start +  i    * cellSize
    let b = start + (i+1) * cellSize
    cells[i] = blockData[a..<b].toSeq()
   
  return cells

# returns the special hash of a network block (this is a Merkle root built on the
# top of the hashes of the 32 cells inside the block)
func hashNetworkBlockOpen( blockData: openArray[byte] ): Hash = 
  let cells  = splitBlockIntoCells(blockData)
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock: hashCell(cells[i]) ))
  return merkleRoot(leaves)

func hashNetworkBlock*(blockData: Block): Hash = hashNetworkBlockOpen(blockData)

#-------------------------------------------------------------------------------

# returns the mini Merkle tree built on the 32 cells inside a network block
func networkBlockTreeOpen( blockData: openArray[byte] ): MerkleTree =
  let cells  = splitBlockIntoCells(blockData)
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock: hashCell(cells[i]) ))
  return merkleTree(leaves)

func networkBlockTree*(blockData: Block): MerkleTree = networkBlockTreeOpen(blockData)

#-------------------------------------------------------------------------------
