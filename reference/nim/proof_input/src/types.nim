
import std/strutils
import std/sequtils

#-------------------------------------------------------------------------------

type Cell*  = seq[byte]
type Block* = seq[byte]

#-------------------------------------------------------------------------------

type 

  MerkleProof*[H] = object
    leafIndex*      : int             # linear index of the leaf, starting from 0
    leafValue*      : H               # value of the leaf 
    merklePath*     : seq[H]          # order: from the bottom to the top
    numberOfLeaves* : int             # number of leaves in the tree (=size of input)

  MerkleTree*[H] = object
    layers*: seq[seq[H]]        
    # ^^^ note: the first layer is the bottom layer, and the last layer is the root

#-------------------------------------------------------------------------------

# the circuit expect merkle path of statically known length, so we need to pad them
func padMerkleProof*[H]( old: MerkleProof[H], newlen: int ): MerkleProof[H] = 
  let pad = newlen - old.merklePath.len
  assert( pad >= 0 )

  var zero : H       # hackety hack hack, it should be initialized to zero

  return MerkleProof[H]( leafIndex:       old.leafIndex       
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

  CellProofInput*[H] = object
    cellData*:    Cell
    merkleProof*: MerkleProof[H]

  SlotProofInput*[H] = object
    dataSetRoot*:    H        # Root
    entropy*:        H        # Entropy
    nSlots*:         int
    nCells*:         int
    slotRoot*:       H        # Root
    slotIndex*:      SlotIdx
    slotProof*:      MerkleProof[H]
    proofInputs*:    seq[CellProofInput[H]]

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
    combo*   : FieldHashCombo
    
  FieldSelect* = enum
    BN254,
    Goldilocks

  HashSelect* = enum
    Poseidon2,
    Monolith

  FieldHashCombo* = enum
    BN254_Poseidon2,
    Goldilocks_Poseidon2,
    Goldilocks_Monolith

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

{. warning[UnreachableElse]:off .}
func toFieldHashCombo*( field: FieldSelect, hash: HashSelect ): FieldHashCombo = 
  let msg = "invalid hash function `" & ($hash) & "` choice for field `" & ($field) & "`"
  case field:
    of BN254: 
      case hash:
        of Poseidon2: return BN254_Poseidon2
        else: raiseAssert(msg)
    of Goldilocks: 
      case hash:
        of Poseidon2: return Goldilocks_Poseidon2
        of Monolith:  return Goldilocks_Monolith
        else: raiseAssert(msg)

#-------------------------------------------------------------------------------
