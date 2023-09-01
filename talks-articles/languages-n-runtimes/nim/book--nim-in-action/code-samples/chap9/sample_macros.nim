import macros

## creating macro to do pre-requisite, no params
macro calculate(): int =
  result = newStmtList(
    infix(
      newIntLitNode(5),
      "*",
      newPar(
        infix(
          newIntLitNode(2),
          "+",
          newIntLitNode(3),
        )
      )

    )
  )

echo(calculate())


## creating macro to act on params
macro arguments(num: int, unknown: untyped): untyped =
  echo "*************"
  result = newStmtList()
  echo num.treeRepr()
  echo unknown.treeRepr()

arguments(10, ["A", "1"])
arguments(10, ["A", 1])      # if not for macro, Arrays must be homogeneous


## shown at compile time
dumpTree:
  5 * (2 + 3)

## to create above AST via code
static:
  var root = newStmtList(
    infix(
      newIntLitNode(5),
      "*",
      newPar(
        infix(
          newIntLitNode(2),
          "+",
          newIntLitNode(3),
        )
      )

    )
  )

  echo(root.repr)
