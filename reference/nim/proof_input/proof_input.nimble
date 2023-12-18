
version       = "0.0.1"
author        = "Balazs Komuves"
description   = "reference implementation for generating the proof inputs"
license       = "MIT or Apache-2.0"
srcDir        = "src"
bin           = @["cli","testmain"]

requires "nim >= 1.6.0"
requires "https://github.com/mratsim/constantine"
requires "https://github.com/codex-storage/nim-poseidon2#9be7b0c134e64e3d57a38520a32af93a55a37c44"
