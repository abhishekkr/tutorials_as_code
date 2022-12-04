### Parallelizing Parser; let's parse output of ls -l

import threadpool, os
import strutils, parseutils


proc parseWords(line: string, wc: var uint) =
  var lineLen = line.len
  if lineLen == 0: return
  var i = 0
  var throwaway: string
  while i < lineLen:
    throwaway.setLen(0)
    i.inc parseUntil(line, throwaway, {' '}, i)
    wc += 1
    if i >= lineLen:
      break
    #echo(throwaway)
    while line[i] == ' ':
      i.inc

proc parseChunk(chunk: string): uint =
  parseWords(chunk, result)

proc wordCount(filename: string, chunkSize = 1_000) =
  var file = open(filename)
  var responses = newSeq[FlowVar[uint]]()
  var buffer = newString(chunkSize)
  var oldBufLen = 0  # length of last buffer that wasn't parsed
  while not endOfFile(file):
    let reqSize = chunkSize - oldBufLen
    # read reqSize chars starting oldBufLen
    let readSize = file.readChars(buffer, oldBufLen, reqSize)  + oldBufLen
    var chunkLen = readSize  # fragment len to parse
    # decreases chunkLen variable until chunkLen-1 points to new line
    while chunkLen >= 0 and buffer[chunkLen - 1] notin NewLines:
      chunkLen.dec
    responses.add(spawn parseChunk(buffer[0..<chunkLen]))
    oldBufLen = readSize - chunkLen
    buffer[0..<oldBufLen] = buffer[readSize - oldBufLen..^1]
  var totalWC: uint = 0
  for resp in responses:
    totalWC += ^resp
  echo totalWC, "\t", filename

if paramCount() > 0:
  wordCount(paramStr(1))
else:
  echo "usage: ", paramStr(0), " <file-path-to-count-word>"
