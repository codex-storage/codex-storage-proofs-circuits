
import types
from types/bn254      import Hash
from types/goldilocks import Digest

import json/bn254
import json/goldilocks

#-------------------------------------------------------------------------------

proc exportProofInput*(hashcfg: HashConfig, fname: string, prfInput: SlotProofInput[Digest]) = 
  exportProofInputGoldilocks(hashcfg, fname, prfInput)

proc exportProofInput*(hashcfg: HashConfig, fname: string, prfInput: SlotProofInput[Hash]) = 
  exportProofInputBN254(hashcfg, fname, prfInput)

#-------------------------------------------------------------------------------
