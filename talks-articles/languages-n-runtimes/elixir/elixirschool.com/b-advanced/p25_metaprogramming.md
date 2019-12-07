
## Metaprogramming

> [sample code](p25_metaprogramming.exs)

### Quote

* elixir's AST is composed of tuples `{:function_name, metadata, fn_args_list}`, using `quote/2` underlying representation can be seen

```
iex> quote do: 10
10

iex> quote do: 10 + 100
{:+, [context: Elixir, import: Kernel], '\nd'}

iex> quote do: if :true, do: :t, else: :f
{:if, [context: Elixir, import: Kernel], [true, [do: :t, else: :f]]}

iex> quote do: fn -> :ok end
{:fn, [], [{:->, [], [[], :ok]}]}
```

* atom, string, numbers, lists and tuples return themselves when `quote`-ed


### Unquote

* to inject new code/values `unquote/1` gets used, `unquote`-ed expression will be evaluated and injected in AST

```
iex> someval = "hi"
"hi"

iex> quote do: fn -> someval <> "ke" end
{:fn, [],
 [
   {:->, [], [[],
              {:<>, [context: Elixir, import: Kernel], [{:someval, [], Elixir}, "ke"]}
    ]}
 ]}

iex> quote do: fn -> unquote(someval) <> "gh" end
{:fn, [],
 [{:->, [], [[], {:<>, [context: Elixir, import: Kernel], ["hi", "gh"]}]}]}
```


### Macros

* macros get replaced with `quote`-ed expression instead of function call

* much of Elixir is itself a macro, macros are defined using `defmacro/2`, check `SampleMacro` module is sample code

* macros replace code, so what code stays can be controlled with conditionals within macros around quote, check `Logger` module in sample code

* when `loud=disabled elixir p25_metaprogramming.exs` infers banner macro as empty as checking system env, `Application.get_env/2` can be used for same


### Debugging

* to understand `quote`-ed code, can use `Macro.to_string/2`

* `MacroDebug.expand` in sample code shows usage of `Macro.expand/2` and `Macro.expand_once/2`

* `defmacrop` can define private macros, need to be defined before code calling them

#### Macro Hygiene

* macros don't conflict with context, are hygienic

* can have unhygienic macros like manipulating values using another macro `var!/2` as in `Unhygienic`

#### Binding

* other than `unquote/1`, can also inject values into code using **variable binding**, include multiple variables avoiding re-evals

* for example check `MacroBinding` for use of `bind_quoted`

---
