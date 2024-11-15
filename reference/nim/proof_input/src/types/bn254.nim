
import std/strutils
import std/bitops
import std/streams

import
  constantine/math/arithmetic,
  constantine/math/io/io_fields,
  constantine/math/io/io_bigints,
  constantine/math/config/curves 

#from constantine/math/io/io_fields import toDecimal

import poseidon2/types
import poseidon2/io
export types

#-------------------------------------------------------------------------------

type BN254_T* = F
type Entropy* = F
type Hash*    = F
type Root*    = Hash

#-------------------------------------------------------------------------------

func intToBN254*(x: int): F = toF(x)

func toDecimalF*(a : F): string =
  var s : string = toDecimal(a)
  s = s.strip( leading=true, trailing=false, chars={'0'} )
  if s.len == 0: s="0"
  return s

func toQuotedDecimalF*(x: F): string = 
  let s : string = toDecimalF(x)
  return ("\"" & s & "\"")

proc writeLnF*(h: Stream, prefix: string, x: F) =
  h.writeLine(prefix & toQuotedDecimalF(x))

proc writeF*(h: Stream, prefix: string, x: F) =
  h.write(prefix & toQuotedDecimalF(x))

#-------------------------------------------------------------------------------

func extractLowBits[n: static int]( A: BigInt[n], k: int): uint64 = 
  assert( k>0 and k<=64 )
  var r : uint64 = 0
  for i in 0..<k:
    let b = bit[n](A, i)     # NOTE: the docunmentation seems to lie about the conventions here....
    let y = uint64(b)
    if (y != 0):
      r = bitor( r, 1'u64 shl i )
  return r

func extractLowBits*(fld: F, k: int): uint64 = 
  let A : BigInt[254] = fld.toBig()
  return extractLowBits(A, k);

#-------------------------------------------------------------------------------

