
#
# export the proof inputs as a JSON file suitable for `snarkjs`
# 

import sugar
import std/strutils
import std/sequtils
import std/streams

from poseidon2/io import elements

import types

#-------------------------------------------------------------------------------

func toQuotedDecimalF(x: F): string = 
  let s : string = toDecimalF(x)
  return ("\"" & s & "\"")

func mkIndent(foo: string): string = 
  return spaces(foo.len)

proc writeF(h: Stream, prefix: string, x: F) =
  h.writeLine(prefix & toQuotedDecimalF(x))

#[
proc writeSeq(h: Stream, prefix: string, xs: seq[F])
  let n      = xs.len
  let indent = mkIndent(prefix)
  for i in 0..<n:
    let str : string = toQuotedF( xs[i] )
    if i==0:
      h.writeLine(prefix & "[ " & str)
    else:
      h.writeLine(indent & ", " & str)
  h.writeLine(indent & "] ")
]#

#-------------------------------------------------------------------------------

type 
  WriteFun[T] = proc (stream: Stream, prefix: string, what: T) {.closure.}

proc writeList[T](h: Stream, prefix: string, xs: seq[T], writeFun: WriteFun[T]) = 
  let n = xs.len
  let indent = mkIndent(prefix)
  for i in 0..<n:
    if i==0:
      writeFun(h, prefix & "[ ", xs[i])
    else:
      writeFun(h, indent & ", ", xs[i])
  h.writeLine( indent & "]" )

proc writeFieldElems(h: Stream, prefix: string, xs: seq[F]) = 
  writeList[F]( h, prefix, xs, writeF )

#-------------------------------------------------------------------------------

proc writeSingleCellData(h: Stream, prefix:string , cell: Cell) = 
  let flds : seq[F] = cell.elements(F).toSeq()
  writeFieldElems(h, prefix, flds)

proc writeAllCellData(h: Stream, cells: seq[Cell]) = 
  writeList(h, "    ", cells, writeSingleCellData )

#-------------------------------------------------------------------------------

proc writeSingleMerklePath(h: Stream, prefix: string, path: MerkleProof) = 
  let flds = path.merklePath
  writeFieldElems(h, prefix, flds)

proc writeAllMerklePaths(h: Stream, cells: seq[MerkleProof]) = 
  writeList(h, "    ", cells, writeSingleMerklePath )

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

proc exportProofInput*(fname: string, prfInput: SlotProofInput) = 
  let h = openFileStream(fname, fmWrite)
  defer: h.close()

  h.writeLine("{")
  h.writeLine("  \"dataSetRoot\":      " & toQuotedDecimalF(prfInput.dataSetRoot) )
  h.writeLine(", \"entropy\":          " & toQuotedDecimalF(prfInput.entropy ) )
  h.writeLine(", \"nCellsPerSlot\":    " & $(prfInput.nCells) )
  h.writeLine(", \"nSlotsPerDataSet\": " & $(prfInput.nSlots) )
  h.writeLine(", \"slotIndex\":        " & $(prfInput.slotIndex) )
  h.writeLine(", \"slotRoot\":         " & toQuotedDecimalF(prfInput.slotRoot) )
  h.writeLine(", \"slotProof\":")
  writeSingleMerklePath(h, "    ", prfInput.slotProof )
  h.writeLine(", \"cellData\":")
  writeAllCellData(h, collect( newSeq , (for p in prfInput.proofInputs: p.cellData) ))
  h.writeLine(", \"merklePaths\":")
  writeAllMerklePaths(h, collect( newSeq , (for p in prfInput.proofInputs: p.merkleProof) ))
  h.writeLine("}")

