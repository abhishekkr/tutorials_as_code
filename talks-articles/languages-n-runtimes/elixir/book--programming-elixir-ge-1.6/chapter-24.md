
## Chapter.24 Protocols - Polymorphic Functions

> code samples for this at [chapter-24.exs](./chapter-24.exs)

> Behavior is internal to a module, Protocol's implementation is completely outside.
>
> Meaning you can extend module's functionality without having to add code to them. Even without having module's Source.

> check `IO.Inspect` protocol and implementation [here](https://github.com/elixir-lang/elixir/blob/v1.12.0/lib/elixir/lib/inspect.ex)

### Defining a Protocol

* defined with `defprotocol`, can have doc and one or more functions

* these functions will not have bodies, just declare interface that protocol requires

```
## example of inspect
defprotocol Inspect do
  @fallback_to_any true
  def inspect(thing, opts)
end
```

> function def will be implementated separately


### Implementing a Protocol

* `defimpl` to implement protocol for 1+ types

```
defimpl Inspect, for: PID do
  def inspect(pid, _opts) do
    "#PID" <> IO.iodata_to_binary(:erlang.pid_to_list(pid))
  end
end

defimpl Inspect, for: Reference do
  def inspect(ref, _opts) do
    "#Ref" ++ rest = :erlang.ref_to_list(ref)
    "#Reference" <> IO.iodata_to_binary(rest)
  end
end
```


### The Available Types

* can define implementation for one or more of: `Any`, `Atom`, `BitString`, `Float`, `Function`, `Integer`, `List`, `Map`, `PID`, `Port`, `Record`, `Reference`, `Tuple`

> * `BitString` in place of `Binary`
>
> * `Any` is catchall, using it shall add `@fallback_to_any true` to Protocol def
>
> can list multiple types in a single `defimpl` as for `Collection` Protocol in code


### Protocols and Structs

* Elixir has user-defined types using structs & few conventions

* struct value is just a map with key `__struct__` referencing struct's module; as seen in `Blob`


### Built-in Protocols

* Elixir comes with following protocols: `Enumerable`, `Collectable`, `Inspect`, `List.Chars`, `String.Chars`

* parse MIDI file to use these protocols

> * file has sequence of variable-length frames
>
> * each frame got 4-character type, a 32-bit `length`, then `length` bytes of data
>
> [chapter-24-parse-midi.exs](./chapter-24/chapter-24-parse-midi.exs) gives a module that represents MIDI file content as a struct which gets used as protocols; file also defines a submodule for individual frame structure

* `Enumerable` protocol is basis for `Enum`; any type implementing it can be used as Argument to Enum

> implementing `Enumerable` for `Midi` structure
>
> * `Enumerable` protocol is defined in four functions `count`, `member?`, `reduce`, `slice`
>
> * implementing `reduce` enables use of `Enum.take/2`
>
> * if Collection is countable, return `{:ok, count}`; if it's read lazily, return `{:ok, __MODULE__}`
>
> * `member?` & `slice` depend on Collection characteristic; when our Protocol returns error it tells `Enumerable` to fallback on naive algorithm
>
> now all Enum functions are available for `Midi`

* Collectable: `Enum.into/2` takes an enumerable and creates new collection; kin'of works opposite to `Enum.reduce`

```
iex(1)> 1..5 |> Enum.into([])
[1, 2, 3, 4, 5]
iex(2)> [{:a, 1}, {:b, 10}] |> Enum.into(%{})
%{a: 1, b: 10}
```

> `defimpl Collectable` does the job

#### Built-in Protocols: Inspect

> protocol used to inspect a value, returning a representation as valid Elixir literal; can just delegate to default
>
> let's implement the protocol for `Midi` & `Midi.Frame` type

#### Better Formatting with Algebra Documents

> MIDI stream formatting has no indentation or reasonable line wrapping; for it use feature of `algebra documents` representing data to be pretty-printed
>
> * `inspect` function return an algebra document instead of string as in [chapter-24-parse-midi-algebra.exs](./chapter-24/chapter-24-parse-midi-algebra.exs)

#### Built-in Protocols: List.Chars and String.Chars

* `List.Chars` protocol used by `Kernel.to_charlist`

* `String.Chars` protocol used by String interpolation; to converting value to a string


### Protocols Are Polymorphism

* Protocols give a tidy way to Polymorphism; packaging varieties of data into Structs and type based functionality to Protocols


---
