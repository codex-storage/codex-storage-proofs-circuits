
#import std/strutils
import std/bitops
import std/streams

import goldilocks_hash/types
export types

#-------------------------------------------------------------------------------

type Goldi_T* = F
type Entropy* = Digest
type Hash*    = Digest
type Root*    = Hash

#-------------------------------------------------------------------------------

func toDecimalF*(a : F): string =
  var s : string = $fromF(a)
  return s

func toQuotedDecimalF*(x: F): string = 
  let s : string = toDecimalF(x)
  return ("\"" & s & "\"")

proc writeLnF*(h: Stream, prefix: string, x: F) =
  h.writeLine(prefix & toQuotedDecimalF(x))

proc writeF*(h: Stream, prefix: string, x: F) =
  h.write(prefix & toQuotedDecimalF(x))

#-------------------------------------------------------------------------------

func extractLowBits*(fld: F, k: int): uint64 = 
  assert( k>0 and k<=56 )
  let val  : uint64 = fromF(fld)
  let mask : uint64 = (1'u64 shl k) - 1
  return bitand(val, mask)

#-------------------------------------------------------------------------------

func digestToJsonString*( d: Digest ): string = 
  let xs: F4 = fromDigest(d) 
  return "[ " & toQuotedDecimalF(xs[0]) & ", " &
                toQuotedDecimalF(xs[1]) & ", " &
                toQuotedDecimalF(xs[2]) & ", " &
                toQuotedDecimalF(xs[3]) & " ]"

#-------------------------------------------------------------------------------
