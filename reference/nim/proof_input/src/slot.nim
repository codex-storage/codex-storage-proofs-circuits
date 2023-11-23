
import sugar

import std/streams
import std/sequtils

import types
import blocks

#-------------------------------------------------------------------------------
# Example slot configuration
#

const exSlotCfg = 
  SlotConfig( nCells:   256  # 1024
            , nSamples: 5    # 20
            , dataSrc:  DataSource(kind: FakeData, seed: 12345)
            )

#-------------------------------------------------------------------------------

{.overflowChecks: off.}
func genFakeCell(cfg: SlotConfig, seed: Seed, idx: CellIdx): Cell  =
  let seed1 : uint64 = uint64(seed)
  let seed2 : uint64 = uint64(idx)
  var cell : seq[byte] = newSeq[byte](cellSize)
  var state : uint64 = 1
  for i in 0..<cellSize:
    state = state*state + seed1*state + (seed2 + 17)
    cell[i] = byte(state)
  return cell

#[
--
-- the Haskell version, for reference:
--
genFakeCell :: SlotConfig -> Seed -> CellIdx -> CellData
genFakeCell cfg seed idx = (mkCellData cfg $ B.pack list) where
  list = go (fromIntegral $ _cellSize cfg) 1 
  seed1 = fromIntegral seed :: Word64
  seed2 = fromIntegral idx  :: Word64
  go :: Word64 -> Word64 -> [Word8]
  go 0   _     = []
  go cnt state = fromIntegral state' : go (cnt-1) state' where
    state' = state*state + seed1*state + (seed2 + 17)
]#

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
