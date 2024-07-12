
import sugar

import std/math
import std/sequtils

#import constantine/math/arithmetic
import constantine/math/io/io_fields
import constantine/math/io/io_bigints
#import constantine/serialization/codecs

import poseidon2/types
import poseidon2/io
#import poseidon2/compress
import poseidon2/merkle
import poseidon2/sponge

#-------------------------------------------------------------------------------

proc testVectorsSponge() = 
  echo( "" )
  echo( "NIM | test vectors for sponge of field elements with rate=1" )
  echo( "-----------------------------------------------------------" )
  for n in 0..8:
    let input : seq[F] = collect( newSeq , (for i in 1..n: toF(i)) )
    let hash = Sponge.digest(input, rate = 1)
    echo( "hash of [1.." & ($n) & "] : seq[F] =  " & toDecimal(hash) )

  echo( "" )
  echo( "NIM | test vectors for sponge of field elements with rate=2" )
  echo( "-----------------------------------------------------------" )
  for n in 0..8:
    let input : seq[F] = collect( newSeq , (for i in 1..n: toF(i)) )
    let hash = Sponge.digest(input, rate = 2)
    echo( "hash of [1.." & ($n) & "] : seq[F] =  " & toDecimal(hash) )

#-------------------------------------------------------------------------------

proc testVectorsHash() = 
  echo ""
  echo "NIM | test vectors for hash (padded sponge with rate=2) of bytes"
  echo "----------------------------------------------------------------"
  for n in 0..80:
    let input : seq[byte] = collect( newSeq , (for i in 1..n: byte(i)) )
    let hash = Sponge.digest(input, rate=2)
    echo( "hash of [1.." & ($n) & "] : seq[byte] =  " & toDecimal(hash) )

#-------------------------------------------------------------------------------

proc testVectorsMerkle() =
  echo ""
  echo "NIM | test vectors for Merkle roots of field elements"
  echo "-----------------------------------------------------"
  for n in 1..40:
    let input : seq[F] = collect( newSeq , (for i in 1..n: toF(i)) )
    let root = Merkle.digest(input)
    echo( "Merkle root of [1.." & ($n) & "] : seq[F] =  " & toDecimal(root) )

  echo ""
  echo "NIM | test vectors for Merkle roots of sequence of bytes"
  echo "--------------------------------------------------------"
  for n in 0..80:
    let input : seq[byte] = collect( newSeq , (for i in 1..n: byte(i)) )
    let root = Merkle.digest(input)
    echo( "Merkle root of [1.." & ($n) & "] : seq[byte] =  " & toDecimal(root) )

#-------------------------------------------------------------------------------

when isMainModule:
  testVectorsSponge()
  testVectorsHash()
  testVectorsMerkle()

#-------------------------------------------------------------------------------
