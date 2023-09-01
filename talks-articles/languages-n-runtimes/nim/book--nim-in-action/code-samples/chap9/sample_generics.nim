import macros

type
  Person = object
    name: string
    age: int

static:
  for sym in getType(Person)[2]:
    echo(sym.strVal)

# $ nim c code-samples/chap9/echo_fields.nim
# ............................................................
# name
# age


# Generics

proc myMax[T] (a, b: T): T =
  if a < b:
    return b
  else:
    return a

doAssert myMax(5, 10) == 10
doAssert myMax(25.5, 10.25) == 25.5

type
  Container[T] = object
    empty: bool
    value: T

proc initContainer[T](): Container[T] =
  result.empty = true

var myBox = initContainer[string]()       # specifying '[type]' is mandatory
doAssert myBox.value == ""
doAssert myBox.empty == true

echo myMax("ahoy", "matey")

# to only support int & float
proc maxNum[T: int | float] (a, b: T): T =
  if a < b:
    return b
  else:
    return a

echo maxNum(10, 100)

type
  Number = int | float | uint

proc isPositive[T: Number] (a: T): bool =
  return a > 0

echo isPositive(10)
echo isPositive(-10)

type
  Comparalable = concept a
    (a < a) is bool

proc max(a, b: Comparalable): Comparalable =
  if a < b:
    return b
  else:
    return a

doAssert max(1, 10) == 10
