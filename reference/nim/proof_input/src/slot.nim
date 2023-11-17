
import sugar

import std/streams
import std/sequtils

import types
import blocks

#-------------------------------------------------------------------------------
# Example slot configuration
#

const exSlotCfg = 
  SlotConfig( nCells:   1024
            , nSamples: 20
            , dataSrc:  DataSource(kind: FakeData, seed: 12345)
            )

#-------------------------------------------------------------------------------

{.overflowChecks: off.}
func genFakeCell(cfg: SlotConfig, seed1: Seed, seed2: CellIdx): Cell  =
  var cell : seq[byte] = newSeq[byte](cellSize)

  var state : int64 = 0
  for i in 0..<cellSize:
    state = state*state + seed1*state + (seed2 + 17)
    cell[i] = byte(state)

  return cell

#-------------------------------------------------------------------------------

proc loadCellData*(cfg: SlotConfig, idx: CellIdx): Cell =
  case cfg.dataSrc.kind

    of FakeData:
      return genFakeCell(cfg, cfg.dataSrc.seed, idx)

    of SlotFile:
      let stream = newFileStream(cfg.dataSrc.filename, mode = fmRead) 
      defer: stream.close()

      stream.setPosition( cellSize * idx )
      var arr : array[cellSize, byte]
      discard stream.readData( addr(arr), cellSize )

      return arr.toSeq()

proc loadBlockData*(cfg: SlotConfig, idx: BlockIdx): Block =
  let cells : seq[seq[byte]] = 
        collect( newSeq , (for i in 0..<cellsPerBlock: loadCellData(cfg, idx*cellsPerBlock+i) ))
  return concat(cells)
      
#-------------------------------------------------------------------------------
