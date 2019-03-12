
## Basic Syntax

* can start a REPL using `lua -i` for interactive mode

* can save program in a file (conventionally with extension `.lua`) and run as `lua ${file}.lua`


### Tokens

* token is either a `keyword`, `identifier`, `constant`, `string literal`, or `symbol`

* following has 3 tokens

```
io.write("Hello from ", _VERSION, "!\n")
```


### Comments

* start with `--[[` and end with `--]]`, as

```
--[[ this wouldn't be run --]]
```


### Identifiers

* identifier for a variable, function or user-defined item

* exceptable identifier examples

```
abc ABC _abc A01bc a_bc abc_
```


### Keywords

* `and`, `or`, `break`, `do`, `end`, `in`

* `if`, `then`, `else`, `elseif`, `false`, `for`, `repeat`, `until`, `while`

* `local`, `function`, `return`

* `true`, `false`, `nil`, `not`

---
