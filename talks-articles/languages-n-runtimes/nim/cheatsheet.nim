## Cheatsheet
## compile as  nim c --threads:on cheatsheet.nim

import os
import strutils, strformat
import locks, tables
import json


let num = 100
let str = "hello"
let flt = 10.1
let bol = true


## concat string
assert str & " world" == "hello world"


## string to int
#import strutils
assert parseInt("99") == 99


## int to string
#import strutils
assert $num == "100"
assert intToStr(num) == "100"
assert $flt == "10.1"
let bolstr = $bol  ## can't use $bol[2]
assert bolstr[2] == 'u'
#import strformat
assert fmt"{bol}" == "true"


## env var
#import os
echo(getEnv("SHELL"))


## magical result variable
proc count10(): int =
  for i in 0..<10:
    result.inc

assert count10() == 10


## get global data from a thread
#import locks, tables
var
  threads: array[4, Thread[void]]
  lock: Lock
  sharedData = {"apikey": "verysafestring"}.toTable
  globalPointer = addr(sharedData)

# using thread pragma
proc threadFoo() {.thread.} =
  acquire(lock)
  let secret = cast[ptr Table[string, string]](globalPointer)[]
  echo(secret["apikey"])
  release(lock)

initLock(lock)
for i in 0..high(threads):
  createThread(threads[i], threadFoo)
joinThreads(threads)
deinitLock(lock)


## string trimming
echo(str[0.. ^1])  ## [.. ^1] works but is deprecated
echo(str[0.. ^2])
echo(str[0.. ^3])


## sequences
var evens = newSeq[int]()
evens.add(2)
evens.add(4)
var odds = @[1, 3]
assert 1 in odds
assert evens.contains(2)
for idx, num in evens:
  echo(num + odds[idx])

evens.add(6)
echo(evens)
evens.delete(1)
echo(evens)


## maps
#import tables
var myMap = initTable[string, string]()
myMap["john"] = "John Doe"
myMap["jane"] = "Jane Doe"

var urMap = {
  "john": "John Doe"
}.toTable

assert myMap["john"] == urMap["john"]
assert myMap.hasKey("jane")

proc displayMap(m: var Table[string, string]) =
  for k, v in m:
    echo(fmt"{k}'s full name is {v}")

myMap.del("john")
displayMap(myMap)


## json
type
  User = object
    name: string
let ff = parseJson("1.1")
let john = parseJson("{\"name\": \"John Doe\"}")
assert ff.kind == JFloat
assert john.kind == JObject
assert john["name"].kind == JString
assert john["name"].str == "John Doe"
assert john["name"].getStr("Jane") == "John Doe"
try:
  assert john["age"].getStr("100") == "100"
except KeyError:
  echo("KeyError raised for missing/wrong")
let johnathan: User = User(name: john["name"].str)
echo(johnathan)


## variant type
type
  Character = object
    case human: bool
    of true:
      name: string
    else:
      discard

let alice = Character(human: true, name: "Alice")
#let fakeAlice = Character(human: false, name: "FakeAlice")  ## not allowed
let shoes = Character(human: false)
echo(alice.name)
try:
  echo(shoes.name)
except:
  echo("shoes got no name")


## async future
import asyncdispatch

var futureV = newFuture[int]()
doAssert (not futureV.finished)

futureV.callback =
  proc (f: Future[int]) =
    echo("future is no longer empty, ", f.read)

futureV.complete(42)
doAssert futureV.finished


echo("passed.")
