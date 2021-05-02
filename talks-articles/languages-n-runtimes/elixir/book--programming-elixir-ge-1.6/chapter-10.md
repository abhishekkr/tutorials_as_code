
## Chapter.10 Processing Collections - Enum and Stream

> usage examples at [chapter-10.exs](./chapter-10.exs)

* iterables implement `Enumeration` protocol, `Enum` module is really powerful for regular uses


### Enum: Processing Collections

* `Collectibles.enum` has examples for

> * `to_list` convert collection to list; `concat` joining collections
>
> * `map` a function over a collection elements to create a new
>
> * `at` for a specific index element; `filter` & `reject` to select elements based on condition

> `ListRecursion` module in `chapter-10.exs` implements exercise implementation for Enum's `all?, each, filter, split, take` and `flatten(list)`


### Streams: Lazy Enum

* `Enum` module is greedy; loads all collection to consume

* to process elements in collection when need arise; use Streams

#### A Stream is a Composable Enumerator

* simple example is `s = Stream.map([1,2,3,5], &(&1*10))` gives a stream

> `UseStreams.try/0` in code-example shows some usages

* since Streams are enumerable, they can be passed to each other as well

* `IO.stream` converts IO device into a stream served one line at a time

* following doesn't use an intermediate storage, but runs twice slower; although this approach proves efficient for remote servers/sensors providing data slower and longer duration (Streams can start processing as soon the first line arrives)

```
File.stream!("/path/to/fyl") |> IO.puts() |> Enum.max_by(&String.length/1)
```

#### Infinite Streams

* lazy streams don't need whole collection to be available before starting processing so can start processing long ranges or infinite ones in same time as smaller ranges

> example comparison at `UseStreams.vs_enum/0`

#### Creating your own Streams

* solely implemented in Elixir libs, no specific runtime support; use helpful wrapper funtions from `Stream` to do heavy lifting

* `Stream.cycle`, cycles through enumerable stream elements

* `Stream.repeatedly`, takes a function and run it each time a new value is required to stream

* `Stream.iterate`, generates an infinite stream taking a first value and next-func

* `Stream.unfold`, create streams where each value is some function of previous state. Similar to iterate, supply start-value and next-func which returns 2 value `{stream_value, new_state}` tuple with first as output and second as input for next run. Stream terminates when next-func returns `nil`.

> example usage for above are at `UseStreams.wrap/0`

* `Stream.resource`, build upon `unfold` where start-value is a resource.. it's not to be opened until value delivery is required so it takes a function that returns the resource handler. Then second is accumulator which uses handler to access resource. When it's done the third function takes care of cleaning up using that handler.

> example usage for `Stream.resource` at `TickTock`
>
> * as in sample below for `Stream.resource`, manages opening-reading-closing of file via Streams

```
Stream.resource(fn -> File.open!("sample") end,
                fn file ->
                  case IO.read(file, :line) do
                    data when is_binary(data) -> {[data], file}
                    _ -> {:halt, file}
                  end
                end,
                fn file -> File.close(file) end)
```


#### Streams in Practice

* not every iteration requires a Stream, consider using it when you want to defer processing until you need output or when dealing with really large enumerables


### The Collectable Protocol

* `Enumerable` protocol lets you iterate over elements; `Collectable` allows to build a collection by inserting elemtns into it

* not all collections are `Collectable` like Ranges can't have new entries added

> lists, binaries, functions, maps, files, hash, dicts, hash sets and IO streams implement it popularly

* the Collectable API can be accessed via `Enum.into` or while using `comprehensions`; usage at `UseStreams.cc/0`

```
Enum.into 1..5, [] ## [1,2,3,4,5]

## Collectable protocol is deprecated for non-empty lists
## Enum.into 1..5, [10, 20] ## [10, 20, 1, 2, 3, 4, 5]
## so use things like Enum.to_list(1..5) ++ [10,20]
```

* Output streams are collectable, so following lazily copies over

```
IO.stream(:stdio, :line) |> Enum.into(IO.stream(:stdio, :line))
```

* check how to manually use Collectable [here](https://hexdocs.pm/elixir/Collectable.html#module-examples)


### Comprehensions

> usage at `UseStreams.cc/0`

* general syntax `result = for generator or filter... [ , into: value ] , do: expression`

```
## simple example
for idx <- [10,20,30], do idx*idx
```

* if there are 2 generators, their operations get nested; can use former generator in latter

```
for idx <- [10,20,30], idy <- [1,10], do idx*idx   ## [10,100,20,200,30,300]
```

* filter acts as gatekeeper for elements, when false moves to next iteration without generating output

* works on Bitstring (by extension on binary and string); syntax changes as generator is enclosed in `<< .. >>`, indicating a Binary

> we can use Binary pattern matching for Bits, since still LHS to `<-` is a pattern

* **SCOPING**; variable assignments inside a comprehension are local to it, can't affect outer assignees

* **RETURNS** a list of values returned by `do expr` in each iteration; this can be changed using `into:` parameter; as in last 3 examples in usage example. Using `into:` can bring all Collectable protocol implementing types.

> `ListRecursion7n8` module in example applies exercise for List and Recursion 7 and 8.

### Moving Past Divinity

> work out when to recurse when to enum

---
