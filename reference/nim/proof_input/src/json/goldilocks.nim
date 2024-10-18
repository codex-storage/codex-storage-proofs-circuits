
#
# export the proof inputs as a JSON file suitable for `snarkjs`
# 

import sugar
#import std/strutils
#import std/sequtils
import std/streams

import goldilocks_hash/marshal

import ../types
import ../types/goldilocks
import shared

#-------------------------------------------------------------------------------

func bytesToFieldElements( bytes: openArray[byte] ): seq[F] = 
  let digests = padAndDecodeBytesToDigest62(bytes)
  return digestSeqToFeltSeq(digests)

#-------------------------------------------------------------------------------

proc writeFieldElems(h: Stream, prefix: string, xs: seq[F]) = 
  writeList[F]( h, prefix, xs, writeF )

#-------------------------------------------------------------------------------

proc writeSingleCellData(h: Stream, prefix:string , cell: Cell) = 
  let flds : seq[F] = bytesToFieldElements(cell)    # cell.elements(F).toSeq()
  writeFieldElems(h, prefix, flds)

proc writeAllCellData(h: Stream, cells: seq[Cell]) = 
  writeList(h, "    ", cells, writeSingleCellData )

#-------------------------------------------------------------------------------

proc writeSingleMerklePath(h: Stream, prefix: string, path: MerkleProof[Digest]) = 
  let flds : seq[F] = digestSeqToFeltSeq( path.merklePath )
  writeFieldElems(h, prefix, flds)

proc writeAllMerklePaths(h: Stream, paths: seq[MerkleProof[Digest]]) = 
  writeList(h, "    ", paths, writeSingleMerklePath )

#-------------------------------------------------------------------------------

#[
  signal input entropy;                                  // public input
  signal input dataSetRoot;                              // public input
  signal input slotIndex;                                // must be public, otherwise we could prove a different slot

  signal input slotRoot;                                 // can be private input
  signal input nCellsPerSlot;                            // can be private input (Merkle tree is safe)
  signal input nSlotsPerDataSet;                         // can be private input (Merkle tree is safe)

  signal input slotProof[maxLog2NSlots];                 // path from the slot root the the dataset root (private input)

  signal input cellData[nSamples][nFieldElemsPerCell];   // private input
  signal input merklePaths[nSamples][maxDepth];          // private input
]#

proc exportProofInput*(fname: string, prfInput: SlotProofInput[Digest]) = 
  let h = openFileStream(fname, fmWrite)
  defer: h.close()

  h.writeLine("{")
  h.writeLine("  \"dataSetRoot\":      " & digestToJsonString(prfInput.dataSetRoot) )
  h.writeLine(", \"entropy\":          " & digestToJsonString(prfInput.entropy ) )
  h.writeLine(", \"nCellsPerSlot\":    " & $(prfInput.nCells) )
  h.writeLine(", \"nSlotsPerDataSet\": " & $(prfInput.nSlots) )
  h.writeLine(", \"slotIndex\":        " & $(prfInput.slotIndex) )
  h.writeLine(", \"slotRoot\":         " & digestToJsonString(prfInput.slotRoot) )
  h.writeLine(", \"slotProof\":")
  writeSingleMerklePath(h, "    ", prfInput.slotProof )
  h.writeLine(", \"cellData\":")
  writeAllCellData(h, collect( newSeq , (for p in prfInput.proofInputs: p.cellData) ))
  h.writeLine(", \"merklePaths\":")
  writeAllMerklePaths(h, collect( newSeq , (for p in prfInput.proofInputs: p.merkleProof) ))
  h.writeLine("}")

proc exportProofInputGoldilocks*(hashcfg: HashConfig, fname: string, prfInput: SlotProofInput[Digest]) = 
  assert( hashcfg.field == Goldilocks )
  exportProofInput(fname, prfInput)

#-------------------------------------------------------------------------------
