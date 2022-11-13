import std/strutils  # for split, parseInt
import sequtils      # for filter
import future        # for syntactice sugar on Anon Proc

# prior declaration
proc totalRecall(count: int): void

proc totalCall(count: int) =
  echo(count)
  if (count > 100):
    return
  totalRecall(count + 1)

proc totalRecall(count: int) = totalCall(count + 1)

totalCall(91)


# procedures returning no value
proc noreturn  = echo("xlo")
proc xreturn() = echo("xllo")  ## idiomatic
proc xreturn(h: string): void = echo(h & "ello")

noreturn()
xreturn()
xreturn("xh")


# return varities
proc o(): string =
  discard "o"

proc lo(): string =
  return "lo"

proc llo(): string =
  result = "llo"

proc ello(): string =
  "ello"

proc hello(): string =
  var result = "he"
  result.add("llo")     # returns void
  result

proc hello2(): auto =
  var result = "he"
  result & "llo2"

assert o() == ""
assert lo() == "lo"
assert llo() == "llo"
assert ello() == "ello"
assert hello() == "hello"
assert hello2() == "hello2"


## procedure parameters

proc bigger(n1: int, n2: int): int =
  if n1 > n2: n1 else: n2

proc sjoin(s1, s2: string): string =   # same type params
  s1 & s2

proc hey(name: string, say = "Hey"): string =  # default values
  say & " " & name

proc addUp(numbers: varargs[int]): int =
  result = 0
  for num in numbers:
    result += num

# overloading addUp procedure to concat variable count of strings
proc addUp(strs: varargs[string]): string =
  result = ""
  for str in strs:
    if result == "":
      result = str
    else:
      result = result & " " & str

# case statement
proc calc(op: string): int =
  let tokens = split(op)
  let operation = tokens[1]
  let n1 = parseInt(tokens[0])
  let n2 = parseInt(tokens[2])
  case operation
  of "+": return n1 + n2
  of "-": return n1 - n2
  of "*": return n1 * n2
  of "/": return (n1 / n2).int
  else: return -1

assert bigger(100, 10) == 100
assert bigger(10, 100) == 100
assert sjoin("he", "llo") == "hello"
assert hey("John") == "Hey John"
assert hey("John", "Hello") == "Hello John"
assert addUp(1, 2, 3, 4, 5) == 15
assert addUp("happy", "new", "year") == "happy new year"
assert calc("10 + 15") == 25
assert calc("40 - 15") == 25
assert calc("5 * 5") == 25
assert calc("50 / 2") == 25


## Anonymous Procedures

proc tryAnon =
  let nums = @[10, 100, 1000, 500, 250]    # @ symbol creates a sequence
  let moreThan250 = filter(nums, proc (x: int): bool = x > 250)
  let moreThan100 = filter(nums, (x: int) -> bool => x > 100)    # anon proc with sugar
  assert moreThan250 == @[1000, 500]
  assert moreThan100 == @[1000, 500, 250]

proc tweak(num: int, foo: proc(x: int): int): int =
  foo(num)

proc greet(name: string, foo: (x: string) -> string): string =
  foo(name)


tryAnon()
assert tweak(99, (x: int) -> int => x + 1) == 100
assert tweak(9, (x: int) -> int => x + 91) == 100
assert greet("John", (x: string) -> string => "Hey! " & x) == "Hey! John"
assert greet("John", (x: string) -> string => "Bye! " & x) == "Bye! John"

echo("2.2.3 fin.")
