proc printf(f: cstring): cint {.importc, varargs, header: "stdio.h", discardable.}

printf("%d: %s", 1, "Alice")
