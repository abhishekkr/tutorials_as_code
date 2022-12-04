## Parallelism

import threadpool, os
import re, strutils, parseutils


### ThreadPool, spawn

proc checkPrime(num: int): bool =
  sleep(300)
  if num == 1 or num == 2: return true
  if num %% 2 == 0: return false
  var idx = 3
  let numHalf: int = int(num / 2)
  while idx < numHalf:
    if num %% idx == 0:
      echo(num, " divisible by ", idx)
      return false
    idx += 2
  return true

assert checkPrime(2)
assert not checkPrime(25)

discard spawn checkPrime(725)  # as not using returned FlowVar
discard spawn checkPrime(727)
sync()

var n997 = spawn checkPrime(997)
while not n997.isReady:
  echo("n997 not ready yet, will check back in 0.1 second")
  sleep(100)
assert ^n997


## Parser

### Regexp
let pat = re"([^\s]+)\:\/\/([^\s]+)"
let patline = "https://example.com"
var matches: array[2, string]
let start = find(patline, pat, matches)
doAssert start == 0
doAssert matches[0] == "https"
doAssert matches[1] == "example.com"

var splitMatch = patline.split(":")
doAssert splitMatch[0] == "https"
doAssert splitMatch[1] == "//example.com"
splitMatch = "1 2 3".split()
doAssert splitMatch[0] == "1"
doAssert splitMatch[1] == "2"
doAssert splitMatch[2] == "3"

proc parseURI(s: string): array[3, string] =
  var idx = 0
  var token = ""
  idx.inc(parseUntil(s, token, {':', '@'}, idx))
  result[0] = token
  idx.inc # :
  idx.inc # /
  idx.inc # /
  idx.inc(parseUntil(s, token, {'@', ':', '/'}, idx))
  result[1] = token
  idx.inc
  idx.inc(parseUntil(s, token, {'@', ':', '/'}, idx))
  result[2] = token

let puLine = "https://example.com/sample.json"
echo(parseURI(puLine))

proc countWords(line: string): int =
  let pat = re"([A-Za-z]+)"
  var match: array[1, string]
  for token in line.split():
    discard find(token, pat, match)
    if match[0] != "":
      result += 1
    

proc wordsPerLine(filename: string) =
  var idx = 0
  for line in filename.lines:
    idx += 1
    echo("line#", idx, "\t", countWords(line))

wordsPerLine(getCurrentDir() / "chap06.md")

## parsing output of lines from ls -l
type
  Stats = ref object
    filename, user, group, updated_at: string
    bytesize: int
proc `$`(stats: Stats): string =
  "(filename: $#,\t owner: $#.$#,\tsize: $#)" % [
    stats.filename, stats.user, stats.group, $stats.bytesize
  ]

proc newStats(): Stats =
  Stats(filename: "", user: "", group: "", updated_at: "", bytesize: 0)

proc parseStats(line: string, s: var Stats) =
  if line.len == 0: return
  if line[0..5] == "total ": return
  var i = 0

  var throwaway: string
  i.inc parseUntil(line, throwaway, {' '}, i)
  i.inc skipWhitespace(line, i)
  i.inc parseUntil(line, throwaway, {' '}, i)
  i.inc skipWhitespace(line, i)

  s.user.setLen(0)
  i.inc parseUntil(line, s.user, {' '}, i)
  i.inc skipWhitespace(line, i)

  s.group.setLen(0)
  i.inc parseUntil(line, s.group, {' '}, i)
  i.inc skipWhitespace(line, i)

  s.bytesize = 0
  i.inc parseInt(line, s.bytesize, i)
  i.inc skipWhitespace(line, i)

  s.updated_at.setLen(0)
  throwaway.setLen(0)
  i.inc parseUntil(line, throwaway, {' '}, i)
  i.inc skipWhitespace(line, i)
  s.updated_at = throwaway
  throwaway.setLen(0)
  i.inc parseUntil(line, throwaway, {' '}, i)
  i.inc skipWhitespace(line, i)
  s.updated_at = "$# $#" % [s.updated_at, throwaway]
  throwaway.setLen(0)
  i.inc parseUntil(line, throwaway, {' '}, i)
  i.inc skipWhitespace(line, i)
  s.updated_at = "$# $#" % [s.updated_at, throwaway]
  s.filename = line[i..^1]

let ls_l = """-rw-r--r--. 1 guest guest 292348657 Dec  4 09:47 nvim_log_py3_neovim_rpc_server
drwx------. 3 root       root              60 Nov 28 10:53 systemd-private-xyz"""

for line in ls_l.split("\n"):
  var s = newStats()
  parseStats(line, s)
  echo $s


echo("parser checked.")




echo("passed.")
echo("Below is example where thread would crash and crash this.\n")

### Exceptions in Threads

# if raises pragma is used, it shall allow what error can be raised otherwise compile shall fail
proc crash(): string {.raises: [ValueError].} =
  raise newException(ValueError, "BOOM!")

try:
  var boom = spawn crash()
except:
  echo("thread boom")    # wouldn't catch thread crash
sync()
