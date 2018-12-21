
_01:00:05_

## Elixir Tutorial: Learn Elixir in One Video

> by Derek Banas

* [check eg01.ex for basic data types, arithmetic and comparison operators; with conditional logic](./eg01.ex), module filename need to have extension `.ex`

* Syntactical style is a bit similar to `ruby`.

* Like erlang can get exposed methods and details on a method using `<ModuleName>.module_info`.

* Module definition happens in `defmodule <ModuleName> do ... end`.

* Function definition is `def <FunctionName> do .. end`.
> * `IO.get "prompt-message" |> String.trim` for user input.
>
> * `IO.puts "#{message}"` for stdout.

* To run this

```
iex(1)> c("eg01.ex")
[Eg01]

iex(2)> Eg01.main
Your name please? James
Hola James
:ok
```

* Variables have immutable value and inferential value.

* Function definition `data_stuff` shows usage for `int`, `float`, `atom`, `ranges`

> * function `string_stuff` shows `strings` operations
>
> * function `arith_stuff` shows common `arithmetic` operations
>
> * function `compare_stuff` utilizes available conditional logic

---

* [check eg02.ex for composite constructs and related flows](./eg02.ex)

> * function `tuple_stuff` and `list_stuff` showcase primary usage modes of tuples and lists respectively
>
> * it also demonstrates recursion with function `recursive_list_stuff`
>
> * `map_stuff` function with creation and updating elixir maps
>
> * common pattern matching usecases shown via function `pattern_stuff`
>
> * higher order functions utilized via function `hof_stuff`

---

* [check eg03.ex for recursion, enums, more](./eg03.ex)

> * function `recursion_stuff` with few examples
>
> * function `enumerable_stuff` with few list enumerations
>
> * list comprehension examples in function `list_comprehension_stuff`
>
> * error handling in function `error_stuff`
>
> * concurrent tasks via function `spawn_stuff`

---
---
