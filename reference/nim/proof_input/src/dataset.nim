
#import sugar

#import std/streams
#import std/sequtils

import types
import slot
#import blocks

#-------------------------------------------------------------------------------
# Example slot configuration
#

const exDataSetCfg* = 
  DataSetConfig( nCells:   256  # 1024
               , nSamples: 7    # 20
               , nSlots:   5
               , dataSrc:  DataSource(kind: FakeData, seed: 12345)
               )

const exGlobalCfg* = 
  GlobalConfig( maxDepth:       16
              , maxLog2NSlots:  5     
              , cellSize:       256
              , blockSize:      4096          
              )

#-------------------------------------------------------------------------------

{.overflowChecks: off.}
func parametricSlotSeed( seed: Seed, k: SlotIdx): Seed = (seed + 72 + 1001*uint64(k))

func parametricSlotFileName( basefile: string, k: SlotIdx): string = basefile & ($k) & ".dat"

func parametricSlotDataSource( src: DataSource, k: SlotIdx): DataSource = 
  case src.kind
    of FakeData:
      return DataSource(kind: FakeData, seed: parametricSlotSeed(src.seed, k))
    of SlotFile:
      return DataSource(kind: SlotFile, filename: parametricSlotFileName(src.filename, k))

#-------------------------------------------------------------------------------

func slotCfgFromDataSetCfg*( dsetcfg: DataSetConfig, idx: SlotIdx ): SlotConfig =
  assert( idx >= 0 and idx < dsetcfg.nSlots )
  let newDataSrc = parametricSlotDataSource( dsetcfg.dataSrc, idx )
  return SlotConfig( nCells:   dsetcfg.nCells
                   , nSamples: dsetcfg.nSamples
                   , dataSrc:  newDataSrc
                   )

#-------------------------------------------------------------------------------

proc dataSetLoadCellData*(globCfg: GlobalConfig, dsetCfg: DataSetConfig, slotIdx: SlotIdx, cellIdx: CellIdx): Cell =
  let slotCfg = slotCfgFromDataSetCfg( dsetCfg, slotIdx )
  return slotLoadCellData(globCfg, slotCfg, cellIdx)

proc dataSetLoadBlockData*(globCfg: GlobalConfig, dsetCfg: DataSetConfig, slotIdx: SlotIdx, blockIdx: BlockIdx): Block =
  let slotCfg = slotCfgFromDataSetCfg( dsetCfg, slotIdx )
  return slotLoadBlockData(globCfg, slotCfg, blockIdx)

#-------------------------------------------------------------------------------
