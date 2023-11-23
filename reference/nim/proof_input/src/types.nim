
import std/strutils

from constantine/math/io/io_fields import toDecimal

import poseidon2/types
export types

#-------------------------------------------------------------------------------

const cellSize*      : int = 128     # 2048         # size of the cells we prove
const blockSize*     : int = 4096    # 65536        # size of the network block

const cellsPerBlock* : int = blockSize div cellSize

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

type 

  CellProofInput* = object
    cellData*:    Cell
    merkleProof*: MerkleProof

  SlotProofInput* = object
    slotRoot*:       Root
    entropy*:        Entropy
    nCells*:         int
    proofInputs*:    seq[CellProofInput]

#-------------------------------------------------------------------------------

type 

  Seed*     = int
  CellIdx*  = int
  BlockIdx* = int

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

#-------------------------------------------------------------------------------
