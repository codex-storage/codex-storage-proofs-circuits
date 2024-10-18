
import sugar

import std/streams
import std/sequtils

import types
#import blocks

#-------------------------------------------------------------------------------
# Example slot configuration
#

const exSlotCfg = 
  SlotConfig( nCells:   256  
            , nSamples: 5    
            , dataSrc:  DataSource(kind: FakeData, seed: 12345)
            )

#-------------------------------------------------------------------------------

{.overflowChecks: off.}
proc genFakeCell(globcfg: GlobalConfig, cfg: SlotConfig, seed: Seed, idx: CellIdx): Cell  =
  let seed1 : uint64 = uint64(seed) + 0xdeadcafe'u64
  let seed2 : uint64 = uint64(idx)  + 0x98765432'u64
  var cell : seq[byte] = newSeq[byte](globcfg.cellSize)
  var state : uint64 = 1
  for i in 0..<globcfg.cellSize:
    state = state*(state + seed1)*(state + seed2) + state*(state xor 0x5a5a5a5a) + seed1*state + (seed2 + 17)
    state = state mod 1698428844001831'u64
    cell[i] = byte(state)
  return cell

#[
-- original Haskell version:
--
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

proc slotLoadCellData*(globcfg: GlobalConfig, cfg: SlotConfig, idx: CellIdx): Cell =
  case cfg.dataSrc.kind

    of FakeData:
      return genFakeCell(globcfg, cfg, cfg.dataSrc.seed, idx)

    of SlotFile:
      let stream = newFileStream(cfg.dataSrc.filename, mode = fmRead) 
      defer: stream.close()

      const maxcellsize = 16384
      assert( globcfg.cellSize <= maxcellsize )
      stream.setPosition( globcfg.cellSize * idx )
      var arr : array[maxcellsize, byte]
      discard stream.readData( addr(arr), globcfg.cellSize )

      let cell: seq[byte] = collect( newSeq, (for i in 0..<globcfg.cellSize: arr[i] ))
      return cell

proc slotLoadBlockData*(globcfg: GlobalConfig, cfg: SlotConfig, idx: BlockIdx): Block =
  let cells : seq[seq[byte]] = 
        collect( newSeq , (for i in 0..<cellsPerBlock(globcfg): slotLoadCellData(globcfg, cfg, idx*cellsPerBlock(globcfg)+i) ))
  return concat(cells)
      
#-------------------------------------------------------------------------------
