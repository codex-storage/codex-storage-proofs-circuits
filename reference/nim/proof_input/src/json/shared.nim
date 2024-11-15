
#import sugar
#import std/sequtils
import std/strutils
import std/streams

#-------------------------------------------------------------------------------

func mkIndent*(foo: string): string = 
  return spaces(foo.len)

#-------------------------------------------------------------------------------

type 
  WriteFun*[T] = proc (stream: Stream, prefix: string, what: T) {.closure.}

proc writeList*[T](h: Stream, prefix: string, xs: seq[T], writeFun: WriteFun[T]) = 
  let n = xs.len
  let indent = mkIndent(prefix)
  for i in 0..<n:
    if i==0:
      writeFun(h, prefix & "[ ", xs[i])
    else:
      writeFun(h, indent & ", ", xs[i])
  h.writeLine( indent & "]" )

#---------------------------------------

proc writeListList*[T](h: Stream, prefix: string, xs: seq[seq[T]], writeFun: WriteFun[T]) = 
  let n = xs.len
  let indent = mkIndent(prefix)
  for i in 0..<n:

    if i==0:
      h.write( prefix & "[ ")
    else:
      h.write( indent & ", ")

    let ys = xs[i]
    let m = ys.len
    for j in 0..<m:
      if j==0:
        writeFun(h, ""   , ys[j])
      else:
        writeFun(h, " , ", ys[j])

    h.write("\n")
  h.writeLine( indent & "]" )

#-------------------------------------------------------------------------------
