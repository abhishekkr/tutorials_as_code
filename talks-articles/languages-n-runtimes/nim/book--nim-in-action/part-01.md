
## Part 1. Nim Basics

> developed by Andreas Rumpf in 2005


### Chap.01 Why Nim?

* What's Nim?

> * a general purpose compiled language to be efficeint, expressive and elegant.. in that order of priority
>
> * like Python uses spaces to delimit scope and uses words instead of symbols for certain operators
>
> * Nim compiler first translates code in C code; making it well suited to systems programming.. [NimKernel](https://github.com/dom96/nimkernel) is a simple OS written with it
>
> * has a soft real-time GC, allowing to specify time spent collecting memory.. so performance critical applications with time constraints can use it well
>
> * Web Apps could make use of powerful async I/O support; Nim can also compile to JS.

* Core Features

> * Metaprogramming (extensive support, easy for DSLs).
>
> * Style-insensitive var/func/type names; `strutils` provide `"ehlo".to_upper()` same as `"ehlo".toUpper()` because Nim identifies them to be same. Nim considers case of first character, not others. Type name shall start with uppercase and variables with lowercase, by convention.
>
> * Type system rich in features. Statically typed with type inference. Also dynamic type-checking features. It's memory safe unless unsafe types are explicitly used as `ptr`. Pointer arithmetic isn't allowed. Allows Generics.
>
> * Compiler translates to C first and thus by virtue of that gets support on all the platforms that have C support, while making pwerful optimizations over C code. This enables use of existing C/C++ libraries with some wrapper code or using `c2nim`. Nim allows to build libraries compatible with C/C++, so being used via C/C++ FFIs. Can also transpile to ObjectiveC (so native iOS support) & JS (so can write client-side logic for browsers). Android Development using C++ compilation backend. Caches compilation targets for speedier subsequent runs. As an example, Nim gets native interfacting with SQLite(via C), Qt(via C++), iOS(ObjectiveC) and jQuery(via JS).
>
> * Number of GCs to be selected/removed. Like for embedded systems or games, can avoid GCs or specialize.
>
> * Does offer OOP features, have no Class definition construct; can use Metaprogramming to create a class like construct.
>
> * Nim compiler transpiles to C. Then depends on a C compiler for compilation to executable.

* Pros & Cons

> * Pros: Efficient, Language Features, Readability, Platform compatibility. A procedural language with varying support for OOP, Functional, other paradigms. Type safety.
>
> * Cons: Young with less packages, small community.



### Chap.02 Getting Started

#### Nim Syntax

* most commonly used keywords are `if, case, of, when, var, let, proc, type, object, try, except, finally`

* like Python, 2-space indentation gets used to delimit code blocks with optional semicolon.. can split a spanning line after punctuation with next line indented

* can comment `# ...` or multiline with `#[ ..\n.. ]#`, could disable a block with `when false:`

#### Nim Basics

* by convention type starts with Uppercase except built-in types (most defined in `system` module)

> int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64, float, float32, float64, bool, char, string

```
let num = 1
let hex = 0xff
let oct = 0o14
let bin = 0b0101
let nvar: int16 = 100  # explicit typing
let mvar = 100'i8      # type siffux as iX or uX
let fx = 1'f32         # also f64
let fy = 1.0e19
let bt = true
let bf = false
# multiple var definition could be done
let
  cp = 'A'
  cq = '\109'
  cr = '\x79'
# identifiers could be in unicode as well
var 火 = "fire"
let txt = "And they said \"Good Morning\""
let lyns = "lorem
  ipsum"
# import strutils ; echo lyns.unindent  ## unindents start spaces on new lines
```

* `let` creates immutable variable; `var` provides mutable; `const` are immutable as well but their value need to be computable at compile time

> `const` example in [code sample](code-samples/chap2_2_2.nim) compiled as `nim c code-samples/chap2_2_2.nim` then creating binary at same path; here a procedure call executed at compile time to fetch value for `const`.. but not all procedures can be executed at compile time like if they are using FFI

* identifiers are style insensitive; camelCase written are equivalent to snake\_case

* a keyword is used as identifier by stropping like, [style guide](https://github.com/nim-lang/Nim/wiki/Style-Guide-for-Nim-Code) for more context

```
var `var` = "sample"
echo(`var`)
```

* `proc` to define procedure as; `discard` keyword lets compiler know returned value could be ignored

```
proc procName(argName: string): string = "Hey " & argName

procName("Joe")
discard procName("Joey")
```

* procedures must be defined before call; for circular dependency in calls a forward declaration is required, [code sample](./code-samples/chap2_2_3.nim)

> * like a var, default return of proc would be binary zero; `result` var is available as default return
> * allows default values for args, varargs
> * procedure overloading with different args; anonymous procedures as well

#### Collection Types

* `array` (static size), `seq` & `set` are major collection types

> * Array's runtime bound checks could be turned off by removing `--boundChecks`; compiling with `-d:release` for debug build would turn it off
> * can define lower bound of Array as `var arr: array[-10 .. -9, int]` providing 2 size array with `arr[-10], arr[-9]`
> * Array constructor could be used `var arr = ["this", "that"]`

* sequences can grow in size within mem limits and still has bound checks at current length

> * could use constructor `var sq = @[1, 2]` or using `var sq = newSeq[int](2) ; sq[0] = 1 ; sq[1] = 2`
> * `sq.len` for length; `for i in 0..<sq.len:` to get indexes in loop range
> * more proc for sequences in `system` & `sequtils` modules

* sets use `{}`, order doesn't matter and can't access items in it via an index

> code samples for [arrays, sequences & set](code-samples/chap2_3.nim)


#### Control Flow & Exception Handling

> code samples for [if, case, for, while, iterator, raise, try, except](code-samples/chap2_4.nim)


#### User-defined Types

> code samples [object & ref](code-samples/chap2_6.nim)

* Nim features objects, tuples and enums

* Nim objects are similar to C structs and passable over FFI, data types stored on Stack can't be `nil`

> defined with `ref` to object are stored on Heap, called `reference types`; can't modify a non-ref parameter so for malleable types use refs

* objects user `nominative typing` & tuples use `structural typing`

* two compact ways to define a tuple; as long order and type of values match.. type of tuples are considered same

```
type
  A = tuple[x, y: int]
  B = (int, int)
```

* allows tuple unpacking similar to pattern matching in haskell/erlang

* enums are similar to C enumsas well, are ordinal types and usable in case statements or base type of sets

> [Pragmas](http://nim-lang.org/docs/manual.html#pragmas) help process compiler input

---
