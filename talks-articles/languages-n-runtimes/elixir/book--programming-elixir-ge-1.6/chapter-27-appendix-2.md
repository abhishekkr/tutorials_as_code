
## Appendix.2 Type Specifications and Type Checking

### When Specifications are used

* come from Erlang, where every public function is preceded by a `-spec` line

> following is a sample from Elixir parser in Erlang

```
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
  throw({error, {Line, ?MODULE, message}}).
```

* this acts as documentation; tools like `dialyzer` can perform static analysis and report mismatch

* Elixir have `@spec` attribute for it; `IEx` avails you `s` helper to display specs and `t` to show user-defined types; then `dialyzer` can run on compiled `*.beam` files


### Specifying a Type

> * basic types in Elixir are: `any`, `atom`, `float`, `fun`, `integer`, `list`, `map`, `maybe_improper_list`, `none`, `pid`, `port`, `reference`, `struct` and `tuple`
>
> * `any` is set of all values; `none` is empty set; nil can be represented as `nil`

* Collection Types represented as `[type]`; `list` is alias for `[any]`; for non-empty list use `[type,...]`

> Binaries represented using
>
> * `<<>>` empty binary (size 0)
>
> * `<<_::size>>` sequence of size bits called bitstring
>
> * `<<_::size * unit_size>>` sequence of `size` units each `unit_size` bits long
>
> Bitstring is `<<_::_>>` an arbitrarily sized sequence of bits
>
> Binary is `<<_::_*8>>` an arbitrary sequence of 8-bit bytes
>
> Tuples are represented as `{type, type, ...}` or use `tuple`; so `{atom,integer}` same to `tuple{atom,integer}`

* Combining Types

> * range operator `..` with literal integers create a type for that range
>
> * 3 built-in types `non_neg_integer`, `pos_integer` & `neg_integer` can be used
>
> * union operator `(|)` indicates acceptable values being union of arguments
>
> * parentheses to group terms in spec

* Structures are basically maps, so can use `map` which throws away useful info so can define one as

```
defmodule Person do
  defstruct fname: "", lname: ""
  @type t :: %Person{fname: String.t, lname: String.t}
end

## usable as Person.t
```

* Anonymous Functions specified using `(head -> return_type)`; use `...` to mean arbitrary typed arguments

```
## examples
(... -> integer)           ## takes arbitrary params; returns int
(list(integer) -> integer) ## takes list of int, returns int
(() -> Strig.t)            ## takes no param, returns string
(integer, atom -> list(atom)) ## take integer and atom, returns list of atoms
```

* Handling Truthy Values as `as_boolean(T)`, actual value matched of type `T`

* more examples

```
integer|float       ## any number
[{atom,any}]        ## list of key-val pair
list(atom,any)      ## same as above
pos_integer|{:error, String.t} ## positive integer OR a tuple of :error and string
(String.t -> {:cont, atom}) ## anon fn taking string and returning :cont and atom
<<_::_*4>>          ## 4-bit nibble sequence
```


### Defining New Types

* attribute `@type` used as `@type new_type :: type_spec`

> some pre-def built-in types are

```
@type term :: any
@type binary :: <_::_*8>>
@type bitstring :: <_::_*1>>
@type boolean :: false|true
@type byte :: 0..255
@type char :: 0..0x10ffff
@type charlist :: [ char ]
@type list :: [ any ]
@type list(t) :: [ t ]                        ## can parameterize types in a new def
@type number :: integer|float
@type module :: atom
@type mfa :: {module, atom, byte}
@type node :: atom
@type nonempty_charlist :: [ char ]
@type timeout :: :infinity|non_neg_integer
@type no_return :: none
```

* Elixir also has `@typep` & `@opaque` attributes doing same thing, with different visibility


### Specs for Functions and Callbacks

* `@spec` specifies func's param count, types & return-value type

* syntax `@spec fn_name(param1_type,...) :: return_type`; example `@spec size(list) :: non_neg_integer`

* for polymorphic defs; multiple specs for same function name be given if type or count changes


### Using Dialyzer

* statically analyzes compiled code with `debug_info` compiler option set

* add `{:dialyxir, "~> 0.5", only: [:dev], runtime: false}` similar entry to dependencies in Mix; then `mix do deps.get, compile`

* now `mix dialyzer` need to construct a data structure containing all types and APIs in both Erlang and Elixir; checking our code and its interaction with the world.. SLOW but DRY

* rerun `mix dialyzer --no-check`

#### Dialyzer and Type Inference

* it also works on unannotated code as it knows type of built-in functions and can infer your function types

> although Dialyzer does `success typing`, infer most permissive types compatible with code assuming code is correct until finds a contradiction
>
> `@spec` helps constrict types & warnings; though use where really needed

---
