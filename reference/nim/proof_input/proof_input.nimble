
version       = "0.0.1"
author        = "Balazs Komuves"
description   = "reference implementation for generating the proof inputs"
license       = "MIT or Apache-2.0"
srcDir        = "src"
bin           = @["cli"]
#bin           = @["cli","testmain"]

requires "nim >= 1.6.0"
requires "https://github.com/mratsim/constantine#ab6fa6ae1bbbd1b10071a92ec209b381b5d82511"
requires "https://github.com/codex-storage/nim-poseidon2#8a54c69032a741160bbc097d009e45a8b5e4d718"
requires "https://github.com/codex-storage/nim-goldilocks-hash#0d12b1429345c3f359df47171332e8966dc94ed3"
#requires "../../../../nim-goldilocks-hash/"
