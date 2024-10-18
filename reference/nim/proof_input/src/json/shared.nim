
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

#-------------------------------------------------------------------------------
