
## Chapter.6 Modules and Named Functions

> sample code at [chapter-06.exs](./chapter-06.exs)

### Compiling a Module

* example

```
defmodule Misc do
  def sq(n) do
    n * n
  end
end
```

* `iex filepath/filename.exs` or `c "filepath/filename.exs"` on iex


### The Function's Body is a Block

* `do...end` block here is just syntactic sugar, underlying syntax is

* example of `Misc` in sample code


### Function Calls and Pattern Matching

* Patterns shall be in order of most specific earlier to generic latter as in `Factorial.of` in sample code


### Guard Clauses

* in sample code for `Factorial.of/1` and `Factorial.tof/1`, guard clause have been used so negative or fraction don't send call in forever loop

* module `Guard` in sample shows simple use of guard clauses

* only subset of Elixir expressions can be used in Guard Clauses, no local functions can be used need to be a macro

> comparison operators, boolean and negation operators, arithmetic operators, join operators `++` and `<>` with left side as literal, `in` operator, bitwise operators, type-check functions
>
> few other functions as `abs/1`, `bit_size/1`, `byte_size/1`, `div/2`, `elem/2`, `float/1`, `hd/1`, `length/1`, `node/0`, `node/1`, `rem/2`, `round/1`, `self/0`, `tl/1`, `trunc/1`, `tuple_size/1`


### Default Parameters

* `def foo(param \\ "defVal"), do: ":: #{param}"` is a sample where `\\` gets used to provide default value

* module `DefaultParam` in sample code is simple case

* make sure same name function don't conflict due to default params


### Private Functions

* `defp` macro instead of `def` gets used to define private function; can't have some definitions of a function private and other public

* `Factorial.do_tof/2` in sample code is a simple example


### The Amazing Pipe Operator `|>`

* `|>` allows taking result of expression on left and insert as first param to invocation of function on right

* chained pipe-operators can be formed for a cleaner pass-through result and get final return value

* simple example in sample code is under `PipeOp.mapreduce/3`


### Modules

* Modules provide namespaces. Encapsulating functions, macros, structs, protocols and other modules.

* Module nesting in Elixir is illusion, all modules defined at top-level.

```
defmodule Out, do: ( defmodule In, do: (def foo, do: "bar") )
## is just a syntactic sugar for
defmodule Outer.Inner, do: (def foo, do: "bar")
```

#### Directives of Modules

* effect of all 3 is lexically scoped, starts when drective is encountered to end of enclosing scope

* `import` directive brings module's functions/macros in current scope

> * can `import Module, only: [foo: arity, bar: arity]` to fetch specific functions
>
> * can `import Module, except: [foo: arity, bar: arity]` to fetch except specific functions


* `alias` directive creates an alias for module

> * eg. `alias Some.Long.Nested.Module.X, as: MX`
>
> * `alias Some.Long.Nested.Module.X` creates default alias as last part `X`; can also be shortened for multiple aliases as `alias Some.Long.Nested.Module.{X, Y, Z}`


* `require` directive ensures `macro` definitions are available when code is compiled


### Module Attributes

* Elixir modules have associated metadata. Each metadata item is called attribute.

* Accessed with `@` prefixed to attribute name, and value set as `@valid_cmds [:walk, :run]`

* These shall be treated as config/metadata only, not as conventional variables.


### Module Names: Elixir, Erlang and Atoms

* internally module names are just `atom`, Elixir converts module name to atom prepended with Elixir; as `:"Elixir.IO" === IO`


### Calling a Function in Erlang library

* in Erlang variables start with uppercase letter, atom simple lowercase

* Erlang module `timer` as `:timer.tc`

---
