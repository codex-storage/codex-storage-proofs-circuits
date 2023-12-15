
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
  outFile:     string
  entropy:     int

const defGlobCfg = 
  GlobalConfig( maxDepth:       32
              , maxLog2NSlots:  8
              , cellSize:       2048
              , blockSize:      4096          
              )

const defDSetCfg = 
  DataSetConfig( nCells:   256
               , nSamples: 5
               , nSlots:   11
               , dataSrc:  DataSource(kind: FakeData, seed: 12345)
               )

const defFullCfg =
  FullConfig( globCfg:   defGlobCfg
            , dsetCfg:   defDSetCfg
            , slotIndex: 0
            , outFile:   "out.json"
            , entropy:   1234567
            )

#-------------------------------------------------------------------------------

proc printHelp() =
  echo "usage:"
  echo "$ ./cli [options] <proof_input.json>"
  echo ""
  echo "available options:"
  echo " -d, --depth      = <maxdepth>      : maximum depth of the slot tree (eg. 32)"
  echo " -N, --maxslots   = <maxslots>      : maximum number of slots (eg. 256)"
  echo " -c, --cellSize   = <cellSize>      : cell size in bytes (eg. 2048)"
  echo " -b, --blockSize  = <blockSize>     : block size in bytes (eg. 65536)"
  echo " -s, --nslots     = <nslots>        : number of slots in the dataset (eg. 13)"
  echo " -n, --nsamples   = <nsamples>      : number of samples we prove (eg. 100)"
  echo " -e, --entropy    = <entropy>       : external randomness (eg. 1234567)"
  echo " -S, --seed       = <seed>          : seed to generate the fake data (eg. 12345)"
  echo " -f, --file       = <datafile>      : slot data file"
  echo " -i, --index      = <slotIndex>     : index of the slot (within the dataset) we prove"
  echo " -k, --log2ncells = <log2(ncells)>  : log2 of the number of cells inside this slot (eg. 10)"
  echo " -K, --ncells     = <ncells>        : number of cells inside this slot (eg. 1024; must be a power of two)"
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
      if argCtr == 0: fullCfg.outFile = key
      argCtr += 1

    # Switches
    of cmdLongOption, cmdShortOption:
      case key

      of "h", "help"      : printHelp()
      of "d", "depth"     : globCfg.maxDepth      = parseInt(value) 
      of "N", "maxslots"  : globCfg.maxLog2NSlots = ceilingLog2(parseInt(value))
      of "c", "cellSize"  : globCfg.cellSize      = checkPowerOfTwo(parseInt(value),"cellSize")
      of "b", "blockSize" : globCfg.blockSize     = checkPowerOfTwo(parseInt(value),"blockSize")
      of "s", "nslots"    : dsetCfg.nSlots        = parseInt(value)
      of "n", "nsamples"  : dsetCfg.nsamples      = parseInt(value)
      of "e", "entropy"   : fullCfg.entropy       = parseInt(value)
      of "S", "seed"      : dsetCfg.dataSrc       = DataSource(kind: FakeData, seed: uint64(parseInt(value)))
      of "f", "file"      : dsetCfg.dataSrc       = DataSource(kind: SlotFile, filename: value)
      of "i", "index"     : fullCfg.slotIndex     = parseInt(value)
      of "k", "log2ncells": dsetCfg.ncells        = pow2(parseInt(value))
      of "K", "ncells"    : dsetCfg.ncells        = checkPowerOfTwo(parseInt(value),"nCells")
      else:
        echo "Unknown option: ", key
        echo "use --help to get a list of options"
        quit()

    of cmdEnd:
      discard  

  if argctr != 1:
    echo "expecting exactly 1 position argument, the name of the output JSON file"
    echo "use --help for getting a list of options"
    quit()

  fullCfg.globCfg = globCfg
  fullCfg.dsetCfg = dsetCfg

  return fullCfg

#-------------------------------------------------------------------------------

when isMainModule:

  let fullCfg = parseCliOptions()
  # echo fullCfg

  let prfInput = generateProofInput( fullCfg.globCfg, fullCfg.dsetCfg, fullCfg.slotIndex, toF(fullCfg.entropy) )
  exportProofInput( fullCfg.outFile , prfInput )
