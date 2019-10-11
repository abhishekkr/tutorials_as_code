
## Chapter.5 Anonymous Functions

* anon functions created using `fn` keyword,  `fn (parameter-list..) -> body end`

```
iex> sum = fn (a, b) -> a + b end
#Function<12.128620087/2 in :erl_eval.expr/5>
iex> sum.(1,2)
3
```

### Functions and Pattern Matching

* even when parameters are passed in function call, they get pattern matched to params not assigned

* also means complex pattern matching can be performed in function definition as swap is simple as `swap = fn { a, b } -> { b, a } end`


### One Function, Multiple Bodies

* `handle_open` in [chapter-05.exs](./chapter-05.exs) passed to `Fnx.run` handles success and failure of opening file with different return tuples

> in `handle_open`, used `:file.format_error` refers to Erlang `File` module

* cannot mix clauses with different arities, so can't have one `fizz_buzz` function handle all cases


### Functions Can Return Functions

* something like following would work as well

```
foo = fn who -> (fn -> "Hello #{who}" end) end
foo.("World!").()
```


### Passing Functions as Arguments

* `Fnx.run` to apply a function with passed param in sample code file

* Examples with Enum

```
lst = [1, 5, 9]
Enum.map lst, fn elem -> elem * 2 end
```

#### Pinned Values and Function Parameters

* using `^` pin operator allowing to use current value of a variable in a pattern, can be used with function parameters too

* sample code in `Greeter` module


#### The & Notation

* `&` operator to notify which parameter is being referenced; `fizz_buzz` in sample code references same parameter multiple times

* below `greet` gets assigned Anonymous function, but since `speak` definition allows it gets a direct reference from Elixir

```
iex> speak = &(IO.puts(&1))
&IO.puts/1
iex> speak.("donald")
donald
:ok

iex> greet = &(IO.puts("Hello #{&1}"))
#Function<6.128620087/1 in :erl_eval.expr/5>
iex> greet.("donald")
Hello donald
:ok
```

> in definition for `speak` there is a simple call to named function with same order of parameters

* `[]` and `{}` being Elixir operators, literal lists and tuples can also be turned into functions as `divrem = &{ div(&1, &2), rem(&1, &2) }`

* also work with string `hola = &"hola #{&1}"` and string-like as `match_end = &~r/.*#{&1}$/`

* `&` operator with a function name and its arity returns Anonymous Function to call it as `&IO.puts/1` and `&Enum.count/1`


---
