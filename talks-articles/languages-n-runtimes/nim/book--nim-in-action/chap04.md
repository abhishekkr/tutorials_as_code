
## Chapter.4 Standard Library

> follows a batteries included philosophy like Python
>
> [source](code-samples/chap4.nim)

```
# some of most useful pure modules
core:                     system (threads, channels), locks, threadpool, macros
collection & alogirthms:  algorithm, tables, sets, sequtils
string handling:          strutils, parseutils, strtabs, unicode, pegs
OS services:              os, osproc, times, asyncfile
parsers:                  parseopt, parsecfg, json, xmlparser, htmlparser
internet protocols:       httpclient, asynchttpserver, uri, asyncnet, net
other modules: math, hashes, md5, colors, logging, unittest, marshal, future

# most useful impure modules
re, db_mysql, db_sqlite, db_postgres
```

### 4.1 A closer look at modules

* Nim compiler looks a specific set of directories for modules, generally configured at `nim.cfg`

> compiler may use multiple config files, one gets defined by compiler.. usually in `$nimDir/config`

* Can create a project specific config file by creating `main.nims` if `main.nim` is compiled file; compiler searches for files alongside module

* can also place a module in sub-dir as `helper/mydata.nim` and use as `import helper/mydata`

* namespaces act as context identifiers, in Nim defined by individual modules allowing UFCS (Uniform Function Call Syntax)

```
proc sayHey(name: string) = echo("Hey ", name, "!")

sayHey("Jane")
"Jane".sayHey()
```

> * if two modules are overloading proc names, the public definitions are placed in module namespace but not always required to mention
> * `from moduleName import nil` avoids loading all public definitions and required to be used as `moduleName.procName`
> * `from moduleName import specificProc` would only load `specificProc` proc name
> * `from moduleName except specificProc` would only not load `specificProc` proc name

### 4.2 Overview of stdlib

* Pure Modules are written completely in Nim without dependencies.

* Impure Modules use external C libraries; are memory safe.

* Wrappers are modules that allow external C libs to be used via interface; would need to manage memory manually.

### 4.3 The core modules

* has `system` module that's auto-loaded; provides all primitive types

> * `+`, `-`, `*`, `/`, `==`, `!=`, `>`, `<`, `>=`, `<=`, `and`, `not`, `or`
> * `add`, `len`, `shl` shift left, `shr` shift right, `&` concat operator
> * `quit` terminates app with a specified error code, `$` convert to string
> * `repr` works on any type for string representation as `5.repr`
> * `substr` returns a slice of specified string as `"something".substr(0, 2)`
> * `echo` display specified value, `doAssert` and `assert`
> * `items` as iterator that loops through a sequence or string

* `system` module includes `threads` and `channels` modules

* `macros`, `threadpool` and `locks` are in core category

### 4.4 Data Structures and Algorithms

* sequence type `seq[T]` defines dynamic list of type `T`; some structure need `hash table` which support any type key

* `tables` module implement hash tables as `var x = toTable[string, float]({"this": 1.1, "that": 1.1})`

* generic is `Table[A, B]`; then `OrderedTable[A, B]` remembers insertion order; also `CountTable[A]` just counting occurence of each key

> `import hashes` to write/overload hash proc for a type that doesn't have one; `keyStr.hash` defines type and `!$` computes

* `set`'s' (defined in `system` module) base type is limited to an ordinal type of certain size, one of `int8, in16, uint8/byte, uint16, char, enum`

* `sets` module provide `HashSet[A]` without base type limitation and requires a `hash` procedure like `table`; there are also `OrderedSet[A]`

> `O(1)` complexity to determine whether an element is in a Set, `O(n)` for sequences or arrays

* `alogrithm` module with `sort` proc with `sort` proc to sort itself & `sorted` to sort a copy; param goes for sorting type

> can create a `cmp` proc to be passed for custom types with custom comparisons; a simple cmp works for any type with `==` & `<`

* `reverse` proc reverses the order, `fill` proc fills an array with an item

* `sequtil` module has bunch of helpful procs like `filter, map, apply, distribute, zip` along with `delete, insert` and more


### 4.5 Interfacing with OS

* major usage: accessing filesystem, manipulating file/dir paths, retrieving env vars, reading cmd-line args, executing external processes, accessing/managing current system time/date

* `getHomeDir` like os agnostic procs; can join paths with `/` operator or use `joinPath` proc

> `splitPath`, `parentDir`, `tailDir`, `splitFile`, `dirExists`, `fileExists`, `walkDir` and more useful fs helper procs

* module `osproc` enable executing external process with `execCmd` (runs given command and returns exit code without anyway of capturing stdin/out/err), `execCmdEx` (runs and returns exot code and output)

> `_` could be used as discard's placeholder for return when pattern matching a return value (as in Go lang)


### 4.6 Understanding and Manipulating data

* `strutils` module has many string modifying procs; like `toUpperAscii`, `toLowerAscii`, `parseInt`, `startsWith`, `split`, etc.

* parsers are available for cmd-line args, config files like `.ini`, XML, JSON, HTML, CSV, SQL & more as `parseopt`, `parsexml`, etc.

> * some define high level api and low-level parsers both, as for JSON 
> * `xmldom` gives web DOM-like API, `xmltree` module gives Nim idiomatic data

* `..` as slice operator allows picking ranges of indexes as `dat[1..3]`, `dat[3..^2]` (here `^` counts back from end index, `^2` means second last entry)

* `import parseopt` to handle cli args in more ease; `for kind, key, val in getOpt():` providing different args manageable in `case`


### 4.7 Networking and Internet

* async event loop from `asyncdispatch` and async sockets from `asyncnet`; for sync there is `net`

* modules `httpclient`, `smtp`, `asyncftpclient` for direct use; also `asynchttpserver` for high-perf http server allowing it to be a good candidate for web api/services

> `-d:ssl` flag to enable SSL support, even needed for making client calls like over https protocol

---
