
## Chapter.8 Maps, Keyword Lists, Sets and Structs

> sample code at [chapter-08.exs](./chapter-08.exs) & [chapter-08-struct.exs](./chapter-08-struct.exs)


### How to choose between Maps, Structs and Keyword Lists

* to pattern-match against contents like keys, use `map` module

* to allow multiple entries with same key or ordered elements, use `Keyword` module

* for fixed set of fields, use a `struct`


### Keyword Lists

* typically used for options passed to functions

* sample code module `Page.print_text/2`  is an example


### Maps

* go-to key/value data structures, good perf at all sizes


#### Pattern Matching and Updating Maps

* for cases dependent on existence of a key or value, using destructuring

> throws an exception on pattern match failing, say expecting a key named `name` but map not havinng it

* `for` construct lets iterate over map collection as `for person = %{name: name} <- people, name != "dummy", do: person`

* works well in `cond` expressions, functions' head matching

* can't bind keys during pattern matching unlike values

* key need to be used with pin operator `^` inside a pattern as with `PatMatAndMap.need_pin/0`

* `%{_map | key => value, ...}` works to edit existing key's value; need `Map.put_new/3` to add


### Structs

> generic maps don't convey intent; whether it allows whitelist keys or some keys have default values

* provides typed map with default-valued fixed set of fields; pattern-match by type or content

* a module wrapping limited form map where keys must be `atom` & don't have `Dict` abilities

* uses `defstruct` macro to define members, as in example `Contact`

* load up `iex chapter-08-struct.exs` which defines `Contact` and `Movie` structs; run `StructContect` & `StructMovie` to test struct init, update & pattern-match

```
matrix = %Movie{name: "The Matrix", genre: "fantasy"}
zoolander = %Movie{name: "Zoolander", genre: "comedy"}

Movie.comedy?(matrix)
Movie.comedy?(zoolander)
```


### Nested Dictionary Structures

* when a Key in a Struct is itself of Struct/Dict type

> usage can be seen as `Bill` and `StructBill` modules in `iex chapter-08-struct.exs`

* `put_in` to set new value, `update_in` to apply a function at value can be used as in example at `StructBill.change`

#### Nested Accessors and Nonstructs

* can supply keys as atoms

```
bill = %Bill{paid_by: %Contact{name: "Soh Da", phone_number: "0x0"}, amount: 1.5, order_id: "ox0"}

bill[:paid_by][:name] |> IO.inspect()
```

#### Dynamic (Runtime) Nested Accessors

* nested accessors passed till now are macros (operate at compile time), number of keys passed need be static

* `get_in`, `pop_in`, `put_in`, `update_in` & `get_and_update_in` can take a list of keys as parameter; making it a function call not macro

> check the usage at `NestedDict.try/0` in `chapter-08.exs`

```
users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
all = fn :get, data, next -> Enum.map(data, next) end
get_in(users, [all, :age])
```

#### The Access Module

* provides predefined functions to be used as parameters to `get_in` & `get_and_update_in`

* `all` & `at` function is only for lists; `elem` works at tuples; `key` & `key!` at dictionaries and `pop`

> check the usage at `UseAccess.try/0` in `chapter-08.exs`


### Sets

* implemented using module `MapSet`; can instantiate from range like `1..5 |> Enum.into(MapSet.new)`

* have `MapSet.union/2`, `MapSet.difference/2`, `MapSet.intersection/2` available

> can check `UseSets.try/0` at `chapter-08.exs`


### Stay Pure, don't try bringing in Objects using Structs (or Maps) & Modules.

---
