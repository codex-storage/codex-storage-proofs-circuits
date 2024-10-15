
import std/strutils
import std/sequtils

from constantine/math/io/io_fields import toDecimal

import poseidon2/types
export types

#-------------------------------------------------------------------------------

type Entropy* = F
type Hash*    = F
type Root*    = Hash

#-------------------------------------------------------------------------------

func toDecimalF*(a : F): string =
  var s : string = toDecimal(a)
  s = s.strip( leading=true, trailing=false, chars={'0'} )
  if s.len == 0: s="0"
  return s

#-------------------------------------------------------------------------------

type Cell*  = seq[byte]
type Block* = seq[byte]

#-------------------------------------------------------------------------------

type 

  MerkleProof* = object
    leafIndex*      : int             # linear index of the leaf, starting from 0
    leafValue*      : Hash            # value of the leaf 
    merklePath*     : seq[Hash]       # order: from the bottom to the top
    numberOfLeaves* : int             # number of leaves in the tree (=size of input)

  MerkleTree* = object
    layers*: seq[seq[Hash]]        
    # ^^^ note: the first layer is the bottom layer, and the last layer is the root

#-------------------------------------------------------------------------------

# the circuit expect merkle path of statically known length, so we need to pad them
func padMerkleProof*( old: MerkleProof, newlen: int ): MerkleProof = 
  let pad = newlen - old.merklePath.len
  assert( pad >= 0 )
  return MerkleProof( leafIndex:       old.leafIndex       
                    , leafValue:       old.leafValue      
                    , merklePath:      old.merklePath & repeat(zero,pad)
                    , numberOfLeaves:  old.numberOfLeaves
                    )

#-------------------------------------------------------------------------------

type 

  Seed*     = uint64
  CellIdx*  = int
  BlockIdx* = int
  SlotIdx*  = int

  CellProofInput* = object
    cellData*:    Cell
    merkleProof*: MerkleProof

  SlotProofInput* = object
    dataSetRoot*:    Root
    entropy*:        Entropy
    nSlots*:         int
    nCells*:         int
    slotRoot*:       Root
    slotIndex*:      SlotIdx
    slotProof*:      MerkleProof
    proofInputs*:    seq[CellProofInput]

#-------------------------------------------------------------------------------

type 
  DataSourceKind* = enum 
    SlotFile,
    FakeData

  DataSource* = object
    case kind*: DataSourceKind
      of SlotFile: 
        filename*: string
      of FakeData: 
        seed*:     Seed

  SlotConfig* = object
    nCells*   : int           # number of cells per slot (should be power of two)
    nSamples* : int           # how many cells we sample
    dataSrc*  : DataSource    # slot data source

  DataSetConfig* = object
    nSlots*   : int           # number of slots in the dataset
    nCells*   : int           # number of cells per slot (should be power of two)
    nSamples* : int           # how many cells we sample
    dataSrc*  : DataSource    # slot data source

  GlobalConfig* = object
    maxDepth*      : int      # maximum depth of the big merkle tree (log2 of maximum numbers of cells per slot)
    maxLog2NSlots* : int      # log2 of maximum number of slots per dataset
    cellSize*      : int      # size of the cells we prove (2048)
    blockSize*     : int      # size of the network block (65536)

  HashConfig* = object
    field*   : FieldSelect
    hashFun* : HashSelect
    
  FieldSelect* = enum
    BN254,
    Goldilocks

  HashSelect* = enum
    Poseidon2,
    Monolith

#-------------------------------------------------------------------------------

func cellsPerBlock*(glob: GlobalConfig): int = 
  let k = (glob.blockSize div glob.cellSize)
  assert( k * glob.cellSize == glob.blockSize , "block size is not divisible by cell size" )
  return k

#-------------------------------------------------------------------------------

func parseField*(str0: string): FieldSelect =
  let str = strutils.toLowerAscii(str0)
  case str:
    of "bn254":      return BN254
    of "goldilocks": return Goldilocks
    else: raiseAssert("parsefield: unrecognized field `" & str0 & "`")

func parseHashFun*(str0: string): HashSelect =
  let str = strutils.toLowerAscii(str0)
  case str:
    of "poseidon2":  return Poseidon2
    of "monolith":   return Monolith
    else: raiseAssert("parsefield: unrecognized hash function `" & str0 & "`")

#-------------------------------------------------------------------------------
