
## Wrapping Go in Python

> by, Marcus Willock

[presentation](https://github.com/crazcalm/python_go_presentation)

### building shared objects in Go

```go_go_go.go
package main

import "C" /* need it for C Python */
import "fmt"

/*
Below line is important for Go compiler to understand what need to be exported,
also there shouldn't be any whitespace between comment-symbol and `export` word
*/

//export goGoGo
func goGoGo(){
  fmt.Println("Go Go Go, Lock and Load.")
}

func main(){
  goGoGo()
}
```

```build-so
#!/usr/bin/env bash

go build -o go_go_go.so -buildmode=c-shared go_go_go.go
```

---

### calling shared object files from python

```callr_go_go_go.py
import ctypes
import os

def main():
  # Path to shared object file
  path = os.path.join(os.path.abspath("."), "go_go_go.so")

  # load shared object file
  lib = ctypes.CDLL(path)

  # call shared object method
  lib.goGoGo()


if __name__ == "__main__":
  main()
```

---

### testing what is possible

* passing
> * Int
> * String
> * Float
> * Bool
> * List
> * Dict

---

### ending remarks



---
---
