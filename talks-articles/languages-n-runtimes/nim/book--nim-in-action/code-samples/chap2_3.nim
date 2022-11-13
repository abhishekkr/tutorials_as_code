## Collection Types

## Arrays
var days: array[7, string]
days[0] = "sunday"
days[1] = "monday"
assert days[0] == "sunday"
assert days[2] == ""        ## empty yet
echo(days.repr)             ## string representation along with some debug info

## Sequences
var sq: seq[int] = @[]
assert sq == @[]
sq.add(10)
#sq[1] = 11 ## not allowed as index 1 doesn't exist
sq.add(12)
assert sq[1] == 12

var sqq = @[11, 12, 13, 14, 15]
assert sqq[2] == 13
sqq.add(16)
assert sqq[5] == 16

var sqqq = newSeq[int](2)
sqqq[0] = 21
assert sqqq[1] == 0
sqqq[1] = 22
assert sqqq[1] == 22

var sqr = @[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
for i in 0..<sqr.len:
  if sqr[i] mod 2 == 0:
    stdout.write($sqr[i])
    echo("")


## Sets
var ca: set[int16]
assert ca == {}
let cb = {'a', 'e', 'i'}
assert 'i' in cb
let cc = {'A'..'Z'}
assert not ('i' in cc)
assert 'I' in cc

let cd = {'a'..'k'}
let ce = cb * cd
let cf = cd * cb

echo (cb)
echo (cd)
echo (ce)
echo (cf)

echo("..")
