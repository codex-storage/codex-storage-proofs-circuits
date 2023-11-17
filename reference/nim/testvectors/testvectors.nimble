# Package

version       = "0.1.0"
author        = "Balazs Komuves"
description   = "test vectors using the nim-poseidon2 library"
license       = "MIT"
srcDir        = "src"
bin           = @["testvectors"]

# Dependencies

requires "nim >= 1.6.0"
requires "https://github.com/mratsim/constantine"
requires "https://github.com/codex-storage/nim-poseidon2#596f7b18070b44ca0bf305bf9bdf1dc4f6011181"
