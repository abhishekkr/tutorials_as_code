
## Chapter.23 Linking Modules: Behaviors & use

> check what constructs like `use GenServer` do & how can modules extend other modules
>
> code examples are at [chapter-23](./chapter-23)

### Behaviors

* behavior is a list of functions declared by a module for a behavior; module implementing it shall define all associated functions

* behavior def as abstract base class in some OOP langauges

> example: an OTP GenServer implementation must implemet standard set of callbacks (`handle_call`, etc)

* compile time error warnings are given, reducing chance of unexpected runtime error

#### Defining Behaviors

* define behavior with a `@callback` definition

> following is an example of Mix Utility creating SCM behavior for source code control

```
defmodule Mix.SCM do

  @type opts :: Keyword.t

  @callback fetchable? :: boolean

  @callback format(opts) :: String.t
  ...
...
```

> `@callback` defs are using minilanguage (Erlang Type Spec); do include Module & Function level documentation

#### Declarig Behaviors

> now that we have `Mix.SCM` behavior; can use `@behavior` attribute to implement Fossil implementation

```
defmodule Mix.SCM.Fossil do

  @behavior Mix.SCM

  @impl Mix.SCM
  def fetchable?, do: true

  @impl Mix.SCM
  def format(opts), do: opts[:fossil]
  ...

  def wassup, do: :noop
...
```

> after adding behavior construct, a compiler warning will be given if it misses any function implementation

#### Taking it further

> use `@impl` attribute taking `true` or Behavior Module name remedying confusion of which implemented function is for Behavior & which internal


### `use` & `__using__`

* `use` passed a module (alongwith optional arg), invokes a func/macro `__using__` in that module (passing optional arg)

> example: `use ExUnit.Case` gets `test` macro & assertion support
>
> example: `use GenServer` gives both behavior that documents `gen_server` callback & default implementations of those

* `__using__` callback typically a macro as used to invoke code in original module


### Puttig it together - Tracing Method Calls

* let's write a `Tracer` module, `use` in another module to add entry-exit to any `def`

> we'll have a evolving set of modules
>
> * [tracer-v1.ex](./chapter-23/tracer-v1.ex) simply overrides default `def` macro from Kernel with one from `TracerV1`; although will give `undef` errors when implementation calls internal methods as `def` dom't give back a call-able of same name
>
> * [tracer-v2.ex](./chapter-23/tracer-v2.ex) evolves it to run function content from macro
>
> * [tracer-v3.ex](./chapter-23/tracer-v3.ex) evolves it to add Traces
>
> * [tracer-v4.ex](./chapter-23/tracer-v4.ex) evolves it to `use`


### Use `use`

* to avoid redundancy and boilerplate

* to extend functionality of code that you can't change, use protocols

---

Exercise [LinkingModules-BehavioursAndUse-2&3](./chapter-23/chapter-23/exercise-behaviors-and-use.ex)

* using `IO.ANSI.format` enables color coding console output

* hangs and dies on just adding empty guard function def

* when updated `defmacro` for `is_match`, it handles guard

---
