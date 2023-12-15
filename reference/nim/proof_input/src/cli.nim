
{. warning[UnusedImport]:off .}

import sugar
import std/strutils
import std/sequtils
import std/os
import std/parseopt

import constantine/math/arithmetic

import poseidon2/types
import poseidon2/merkle

import types
import blocks
import slot
import dataset
import sample
import merkle
import gen_input
import json
import misc

#-------------------------------------------------------------------------------

type FullConfig = object
  globCfg:     GlobalConfig
  dsetCfg:     DataSetConfig
  slotIndex:   int
  entropy:     int
  outFile:     string
  circomFile:  string
  verbose:     bool

const defGlobCfg = 
  GlobalConfig( maxDepth:       32
              , maxLog2NSlots:  8
              , cellSize:       2048
              , blockSize:      65536          
              )

const defDSetCfg = 
  DataSetConfig( nCells:   256
               , nSamples: 5
               , nSlots:   11
               , dataSrc:  DataSource(kind: FakeData, seed: 12345)
               )

const defFullCfg =
  FullConfig( globCfg:    defGlobCfg
            , dsetCfg:    defDSetCfg
            , slotIndex:  0
            , outFile:    ""
            , circomFile: ""
            , entropy:    1234567
            , verbose:    false
            )

#-------------------------------------------------------------------------------

proc printHelp() =
  echo "usage:"
  echo "$ ./cli [options] --output=proof_input.json --circom=proof_main.circom"
  echo ""
  echo "available options:"
  echo " -d, --depth      = <maxdepth>      : maximum depth of the slot tree (eg. 32)"
  echo " -N, --maxslots   = <maxslots>      : maximum number of slots (eg. 256)"
  echo " -c, --cellsize   = <cellSize>      : cell size in bytes (eg. 2048)"
  echo " -b, --blocksize  = <blockSize>     : block size in bytes (eg. 65536)"
  echo " -s, --nslots     = <nslots>        : number of slots in the dataset (eg. 13)"
  echo " -n, --nsamples   = <nsamples>      : number of samples we prove (eg. 100)"
  echo " -e, --entropy    = <entropy>       : external randomness (eg. 1234567)"
  echo " -S, --seed       = <seed>          : seed to generate the fake data (eg. 12345)"
  echo " -f, --file       = <datafile.bin>  : slot data file"
  echo " -i, --index      = <slotIndex>     : index of the slot (within the dataset) we prove"
  echo " -k, --log2ncells = <log2(ncells)>  : log2 of the number of cells inside this slot (eg. 10)"
  echo " -K, --ncells     = <ncells>        : number of cells inside this slot (eg. 1024; must be a power of two)"
  echo " -o, --output     = <inupt.json>    : the JSON file into which we write the proof input"
  echo " -C, --circom     = <main.circom>   : the circom main component to create with these parameters"
  echo ""

  quit()

#-------------------------------------------------------------------------------

proc parseCliOptions(): FullConfig =

  var argCtr: int = 0

  var globCfg = defGlobCfg
  var dsetCfg = defDSetCfg
  var fullCfg = defFullCfg

  for kind, key, value in getOpt():
    case kind

    # Positional arguments
    of cmdArgument:
      # echo ("arg #" & $argCtr & " = " & key)
      argCtr += 1

    # Switches
    of cmdLongOption, cmdShortOption:
      case key

      of "h", "help"      : printHelp()
      of "v", "verbose"   : fullCfg.verbose       = true
      of "d", "depth"     : globCfg.maxDepth      = parseInt(value) 
      of "N", "maxslots"  : globCfg.maxLog2NSlots = ceilingLog2(parseInt(value))
      of "c", "cellsize"  : globCfg.cellSize      = checkPowerOfTwo(parseInt(value),"cellSize")
      of "b", "blocksize" : globCfg.blockSize     = checkPowerOfTwo(parseInt(value),"blockSize")
      of "s", "nslots"    : dsetCfg.nSlots        = parseInt(value)
      of "n", "nsamples"  : dsetCfg.nsamples      = parseInt(value)
      of "e", "entropy"   : fullCfg.entropy       = parseInt(value)
      of "S", "seed"      : dsetCfg.dataSrc       = DataSource(kind: FakeData, seed: uint64(parseInt(value)))
      of "f", "file"      : dsetCfg.dataSrc       = DataSource(kind: SlotFile, filename: value)
      of "i", "index"     : fullCfg.slotIndex     = parseInt(value)
      of "k", "log2ncells": dsetCfg.ncells        = pow2(parseInt(value))
      of "K", "ncells"    : dsetCfg.ncells        = checkPowerOfTwo(parseInt(value),"nCells")
      of "o", "output"    : fullCfg.outFile       = value
      of "C", "circom"    : fullCfg.circomFile    = value
      else:
        echo "Unknown option: ", key
        echo "use --help to get a list of options"
        quit()

    of cmdEnd:
      discard  

  fullCfg.globCfg = globCfg
  fullCfg.dsetCfg = dsetCfg

  return fullCfg

#-------------------------------------------------------------------------------

proc printConfig(fullCfg: FullConfig) =

  let globCfg = fullCfg.globCfg
  let dsetCfg = fullCfg.dsetCfg

  echo "maxDepth   = " & ($globCfg.maxDepth)
  echo "maxSlots   = " & ($pow2(globCfg.maxLog2NSlots))
  echo "cellSize   = " & ($globCfg.cellSize)
  echo "blockSize  = " & ($globCfg.blockSize)
  echo "nSamples   = " & ($dsetCfg.nSamples)
  echo "entropy    = " & ($fullCfg.entropy)
  echo "slotIndex  = " & ($fullCfg.slotIndex)
  echo "nCells     = " & ($dsetCfg.ncells)
  echo "dataSource = " & ($dsetCfg.dataSrc)

#-------------------------------------------------------------------------------

proc writeCircomMainComponent(fullCfg: FullConfig, fname: string) = 
  let blockTreeDepth     = exactLog2(fullCfg.globCfg.blockSize div fullCfg.globCfg.cellSize)
  let nFieldElemsPerCell = (fullCfg.globCfg.cellSize + 30) div 31

  let params: (int,int,int,int,int) = 
        ( fullCfg.globCfg.maxDepth
        , fullCfg.globCfg.maxLog2NSlots
        , blockTreeDepth
        , nFieldElemsPerCell
        , fullCfg.dsetCfg.nSamples
        )

  let f = open(fname, fmWrite)
  defer: f.close()

  f.writeLine("pragma circom 2.0.0;")
  f.writeLine("include \"sample_cells.circom\";")
  f.writeLine("// SampleAndProven( maxDepth, maxLog2NSlots, blockTreeDepth, nFieldElemsPerCell, nSamples )")
  f.writeLine("component main {public [entropy,dataSetRoot,slotIndex]} = SampleAndProve" & ($params) & ";")

#-------------------------------------------------------------------------------

when isMainModule:

  let fullCfg = parseCliOptions()
  # echo fullCfg

  if fullCfg.verbose:
    printConfig(fullCfg)

  if fullCfg.circomFile == "" and fullCfg.outFile == "":
    echo "nothing do!"
    echo "use --help for getting a list of options"
    quit()

  if fullCfg.circomFile != "":
    echo "writing circom main component into `" & fullCfg.circomFile & "`"
    writeCircomMainComponent(fullCfg, fullCfg.circomFile)

  if fullCfg.outFile != "":
    echo "writing proof input into `" & fullCfg.outFile & "`..."
    let prfInput = generateProofInput( fullCfg.globCfg, fullCfg.dsetCfg, fullCfg.slotIndex, toF(fullCfg.entropy) )
    exportProofInput( fullCfg.outFile , prfInput )

  echo "done"
