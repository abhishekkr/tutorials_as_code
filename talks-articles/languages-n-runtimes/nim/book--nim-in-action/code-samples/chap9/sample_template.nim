import os

template repeat(count: int, statements: untyped) =
  for idx in 0..<count:
    statements

template repeaaat(statements: untyped) =
  while true:
    statements

template decalreVar(varName: untyped, varVal: typed) =
  var varName = varVal

template hygieneSample(varName: untyped) =
  var varName = 8
  var notInjected = 1024
  var injected {.inject.} = notInjected / 64


decalreVar(abc, 100)
echo(abc)

hygieneSample(blah)
doAssert(blah == 8)
doAssert(injected == 16)

# hygieneSample(yada)     ## errors as tries redfine injected


# accepting code block as parameter
repeat 3:
  echo("what")
  sleep(1000)

repeaaat:
  echo("whoa")
  sleep(1000)



