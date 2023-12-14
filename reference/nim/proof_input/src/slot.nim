
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

# (10852671575406741732, 3735945064, 2557891771)

{.overflowChecks: off.}
proc genFakeCell(cfg: SlotConfig, seed: Seed, idx: CellIdx): Cell  =
  let seed1 : uint64 = uint64(seed) + 0xdeadcafe'u64
  let seed2 : uint64 = uint64(idx)  + 0x98765432'u64
  var cell : seq[byte] = newSeq[byte](cellSize)
  var state : uint64 = 1
  for i in 0..<cellSize:
    state = state*(state + seed1)*(state + seed2) + state*(state xor 0x5a5a5a5a) + seed1*state + (seed2 + 17)
    state = state mod 1698428844001831'u64
    cell[i] = byte(state)
  return cell

#[
genFakeCell :: SlotConfig -> Seed -> CellIdx -> CellData
genFakeCell cfg (Seed seed) (CellIdx idx) = (mkCellData cfg $ B.pack list) where
  list = go (fromIntegral $ _cellSize cfg) 1 
  seed1 = fromIntegral seed + 0xdeadcafe :: Word64
  seed2 = fromIntegral idx  + 0x98765432 :: Word64
  go :: Word64 -> Word64 -> [Word8]
  go 0   _     = []
  go cnt state = fromIntegral state'' : go (cnt-1) state'' where
    state' = state*(state + seed1)*(state + seed2) + state*(state `xor` 0x5a5a5a5a) + seed1*state + (seed2 + 17)
    state'' = mod state' 1698428844001831
]#

#-------------------------------------------------------------------------------

proc slotLoadCellData*(cfg: SlotConfig, idx: CellIdx): Cell =
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

proc slotLoadBlockData*(cfg: SlotConfig, idx: BlockIdx): Block =
  let cells : seq[seq[byte]] = 
        collect( newSeq , (for i in 0..<cellsPerBlock: slotLoadCellData(cfg, idx*cellsPerBlock+i) ))
  return concat(cells)
      
#-------------------------------------------------------------------------------
