
#
# helper functions
#

import std/math

#-------------------------------------------------------------------------------

func floorLog2* (x: int): int = 
  var k = -1
  var y = x
  while (y > 0):
    k += 1
    y = y shr 1
  return k

func ceilingLog2* (x: int): int = 
  if (x==0):
    return -1
  else:
    return (floorLog2(x-1) + 1)

func exactLog2*(x: int): int = 
  let k = ceilingLog2(x)
  assert( x == 2^k, "exactLog2: not a power of two" )
  return k

func checkPowerOfTwo*(x: int , what: string): int = 
  let k = ceilingLog2(x)
  assert( x == 2^k, ("`" & what & "` is expected to be a power of 2") )
  return x

# wtf Nim, serously
func pow2*(k: int): int = 2^k

#-------------------------------------------------------------------------------
