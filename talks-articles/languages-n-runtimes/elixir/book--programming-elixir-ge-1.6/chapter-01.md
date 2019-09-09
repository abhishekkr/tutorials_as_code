
## Chapter.1 Take the Red Pill

* command pipeline can operate in paallel, refer `Parallel.pmap/2` from [chapter-01.exs](chapter-01.exs) where a function is applied in parallel to each collection element

* functions are data-transformers as command-line utilities in shell and can be chained together as required; can run millions on single machine and interoperating between 100s of nodes all with message passing


### Running Elixir with Iex

* `iex` gives you with interactive elixir session; can just start as repl and compile/load code(-files), pop `iex chapter-01.exs` or get it injected in a web framework

* to quit `iex` is similar to erlang console as `^g , q`, twice `^c`, or `^\`

* `h` on iex opens helper; with an argument can give help on elixir modules or individual functions like `h(Enum)` and `h(Enum.map)`

* there are many helper functions available like introspection of values `i("hello")`, compile file in cwd or given path with `c/1` and `c/2`, change cwd with `cwd/1`, show exports in a module `exports/1`, recompile given modules source `r/1` and more

* can be used to compile/execute entire projects, hop onto other nodes and access running elixir applications

* can be customized for options available under `h IEx.configure` with file `.iex.exs` in home dir

> * something like `IEx.configure colors: [eval_result: [:cyan, "bright"]]`
>
> * can put in any elixir code in this auto-load file though


### Compile and Run

* `.ex` files are intended to be compiled into bytecode, `.exs` are intended for scripting

* using `c "chapter-01.exs"` would compile and run all code inside it and it's return value would be modules i.e. `[Parallel, ParallelAdd]`

---
