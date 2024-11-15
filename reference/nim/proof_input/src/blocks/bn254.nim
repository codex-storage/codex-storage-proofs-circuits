
import sugar
import std/sequtils

#import poseidon2/types
import poseidon2/io
import poseidon2/sponge
#import poseidon2/merkle

import ../types
import ../types/bn254
#import ../merkle
import ../merkle/bn254

#-------------------------------------------------------------------------------

func merkleTree*( hashcfg: HashConfig, what: openarray[Hash]): MerkleTree[Hash] =
  assert( hashcfg.combo == BN254_Poseidon2 )
  return merkleTreeBN254( what )

#-------------------------------------------------------------------------------

func hashCellOpen( hashcfg: HashConfig, globcfg: GlobalConfig, cellData: openArray[byte] ): Hash = 
  assert( hashcfg.field   == BN254     )
  assert( hashcfg.hashFun == Poseidon2 )
  assert( cellData.len == globcfg.cellSize , ("cells are expected to be exactly " & $globcfg.cellSize & " bytes") )
  return Sponge.digest( cellData, rate=2 )

func hashCell*(hashcfg: HashConfig,  globcfg: GlobalConfig, cellData: Cell): Hash =  hashCellOpen(hashcfg, globcfg, cellData)

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
func hashNetworkBlockOpen( hashcfg: HashConfig,  globcfg: GlobalConfig, blockData: openArray[byte] ): Hash = 
  let cells  = splitBlockIntoCells(globcfg, blockData)
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock(globcfg): hashCell(hashcfg, globcfg, cells[i]) ))
  return merkleDigestBN254(leaves) # Merkle.digest(leaves) # merkleRoot(leaves)

func hashNetworkBlock*( hashcfg: HashConfig, globcfg: GlobalConfig, blockData: Block): Hash = 
  hashNetworkBlockOpen(hashcfg, globcfg, blockData)

#-------------------------------------------------------------------------------

# returns the mini Merkle tree built on the 32 cells inside a network block
func networkBlockTreeOpen( hashcfg: HashConfig, globcfg: GlobalConfig, blockData: openArray[byte] ): MerkleTree[Hash] =
  assert( hashcfg.field == BN254 )
  let cells  = splitBlockIntoCells( globcfg, blockData )
  let leaves = collect( newSeq , (for i in 0..<cellsPerBlock(globcfg): hashCell( hashcfg, globcfg, cells[i]) ))
  return merkleTree(hashcfg, leaves)

func networkBlockTree*( hashcfg: HashConfig, globcfg: GlobalConfig, blockData: Block): MerkleTree[Hash] = 
  networkBlockTreeOpen(hashcfg, globcfg, blockData)

#-------------------------------------------------------------------------------
