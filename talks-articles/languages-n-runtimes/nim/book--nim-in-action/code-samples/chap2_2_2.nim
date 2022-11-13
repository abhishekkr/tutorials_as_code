# chapter 2.2.2 constant example with procedure

proc fillString(): string =
  result = ""
  echo("generating string..")
  for i in 0 .. 4:
    result.add($i)

const count = fillString()
echo(count)
