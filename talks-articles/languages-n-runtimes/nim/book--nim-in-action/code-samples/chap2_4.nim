## Control Flow
import strutils

var lst = @[11, 22, 33, 44, 55]

for idx, i in lst:
  if i >= 25 and i < 51:
    echo(idx, ": " & intToStr(i) & " is in the range")
  elif i < 25:
    echo(intToStr(i) & " is smaller")
  else:
    echo(intToStr(i) & " is bigger")


case lst[0]
of 1..9:
  echo("units")
of 10..99:
  echo("tens")
else:
  echo("~")


let val = if true: 100 else: 101
assert val == 100

#var zeroToNine: seq[int] = @[]
#for i in @[0..9]:
#  zeroToNine.add(i)
var zeroToNine = @[0,1,2,3,4,5,6,7,8,9]

var vol = 0
while vol < 10:
  assert vol == zeroToNine[vol]
  vol.inc

var i = -1
block label1:
  while true:
    while i < 5:
      i.inc
      if zeroToNine[i] == 1: continue
      if zeroToNine[i] == 2: break label1
echo(i)

iterator values(): int =
  var i = 10
  while i <= 100:
    yield i
    i += 10

for v in values():
  echo(i, v)


## Exception Handling
proc itErrs() =
  raise newException(IOError, "Something ain't right.")

proc handleErrs() =
  try:
    itErrs()
  except IOError:
    echo(">> ", getCurrentExceptionMsg())
  except:
    echo("gotta check what failed")

handleErrs()
