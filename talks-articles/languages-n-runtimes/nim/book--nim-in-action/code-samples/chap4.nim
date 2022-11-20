## chapter.4 data structures and algorithms
import tables, hashes, sets, algorithm, sequtils
import os, osproc
import strutils
import parseopt
import asyncdispatch, httpclient


let s1 = "something somewhere"
let i1 = 1000
assert $i1 == "1000"
assert i1.repr() == "1000"


## Tables
let tbl1 = toTable[string, float]({"this": 1.1, "that": 1.2})
assert tbl1["this"] == 1.1

type
  User = object
    name: string

var john = initTable[User, string]()
john[User(name: "John Doe")] = "verysafe"


type
  Admin = object
    apikey: string
## can customize, or add for a type that's not allowed as key
proc hash(x: Admin): Hash =
  result = (x.apikey & "some-algo-result").hash
  result = !$result  # !$ finally computes hash

var johnRoot = initTable[Admin, int]()
johnRoot[Admin(apikey: "verysafe")] = 101


## Sets
let aSet = toHashSet(["Alice", "Bob", "Eve"])
assert "Jane" notin aSet


## Algorithms
var nums = @[11, 33, 55, 44, 22]
nums.sort(system.cmp[int])
assert nums == @[11, 22, 33, 44, 55]

var c1 = "alphabet"
var c2 = c1.sorted(system.cmp[char])
c1.sort(system.cmp[char])
assert c1 == c2

var sx = ["cheerios", "alphabet"]
sx.sort(system.cmp[string])
assert sx == ["alphabet", "cheerios"]

proc cmpUser(x, y: User): int =
  result = system.cmp[string](x.name, y.name)
let jack = User(name: "jack")
let jill = User(name: "jill")
var jackNJill = [jack, jill]
jackNJill.sort(cmpUser)
assert jackNJill == [jack, jill]
jackNJill = [jill, jack]
jackNJill.sort(cmpUser)
assert jackNJill == [jack, jill]

jackNJill.reverse()
assert jackNJill == [jill, jack]

var xfill: array[5, string]
fill(xfill, 0, 4, "a")
assert xfill == ["a", "a", "a", "a", "a"]


## sequtils
let numbers = @[1, 2, 3, 4, 5, 6]
let odd = filter(numbers, proc (x: int): bool = x mod 2 != 0)
assert odd == @[1, 3, 5]

let double = map(odd, proc(x: int): int = x * 2)
assert double == @[2, 6, 10]

var oddx = odd
apply(oddx, proc(x: int): int = x * 2)
assert oddx == @[2, 6, 10]

assert distribute(oddx, 2) == @[@[2, 6], @[10]]
let abseq = @['a', 'b']
assert zip(abseq, oddx) == @[('a', 2), ('b', 6)]
assert zip(oddx, abseq) == @[(2, 'a'), (6, 'b')]


## work with filesystem
let path1 = getHomeDir() / "Desktop" / "nim-temp-file"
var path1Axn = "undef"
if not fileExists(path1):
  writeFile(path1, "testing nim in action" & "\p")   # \p platform specific newline
  path1Axn = "cleanup"

let path1split = splitPath(path1)
echo path1split
assert path1split.tail == "nim-temp-file"
assert parentDir(path1) == path1split.head

assert tailDir("/x/y/z") == "x/y/z"

let sfile = splitFile("/x/y/z.txt")
assert sfile.dir == "/x/y"
assert sfile.name == "z"
assert sfile.ext == ".txt"

assert dirExists("/does/not/exist") == false
assert fileExists("/does/not/exist/file") == false

let dir1 = "/tmp/today"
if dirExists(dir1):
  echo("walking: " & dir1)
  for kind, path in walkDir(dir1):
    case kind
    of pcDir: echo("[+] " & path)
    of pcFile: echo(" |  " & path)
    of pcLinkToDir: echo(" l  " & path)
    of pcLinkToFile: echo("[+l " & path)


## executin external processes
when defined(windows):
  let e = execCmdEx("dir run-that.bat")
  if e.exitCode == 0:
    echo("run-that.bat is available")
else:
  let e = execCmdEx("ls /tmp/today")
  if e.exitCode == 0:
    echo(e.output)
  else:
    echo("ERROR:" & e.output)


## manipulating data
var dat = "this-is-a-long-connected-text"
assert dat[1..4] == "his-"
assert dat[24..^2] == "-tex"
assert dat.toUpperAscii == "THIS-IS-A-LONG-CONNECTED-TEXT"
assert "DAT".toLowerAscii == "dat"
assert parseInt("10") == 10
assert dat.startsWith("this")
assert dat.substr(24) == "-text"  ## removes first 24chars
assert dat.split("-") == @["this", "is", "a", "long", "connected", "text"]
assert "a nut-case".split({' ', '-'}) == @["a", "nut", "case"]

proc cfgArg(k: string) = echo("got a command arg: ", k)
proc cfgOption(k, v: string) = echo(k, ": ", v)
for kind, k, v in getOpt():
  case kind
  of cmdArgument:
    cfgArg(k)
  of cmdLongOption, cmdShortOption:
    cfgOption(k, v)
  of cmdEnd: discard


## network
let httpcli = newAsyncHttpClient()
let exampleGet = waitFor httpcli.get("https://example.com")
if parseInt(exampleGet.status.split(" ")[0]) < 400:
  echo(exampleGet.version, " | ", exampleGet.headers)


if path1Axn == "cleanup":
  echo readFile(path1)
  removeFile(path1)
  echo("cleaned: " & path1)

echo("passed.")
