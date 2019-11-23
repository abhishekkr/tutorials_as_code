
## Erlang Interoperability

> [sample code](p18_erlang_interoperability.exs)

* existing library at BEAM (Erlang VM) are available

### Standard Library

* erlang's stdlib is available, with modules available as lowercase atoms

* other modules available at [docs](http://erlang.org/doc/apps/stdlib/)


### Erlang packages

* Erlang lib can be added to Mix deps as

```
def deps do
  [{:png, github: "yuce/png"}]
end
```

* Can be accessed as

```
png = :png.create(%{
  :size => {30, 30}, :mode => {:indexed, 8}, :file => file, :palette => palette
})
```


### Notable Differences

> gotchas for interoperability

* `:example` an atom in Elixir, in Erlang atom would be plain lower-snake-case as `example.`

* Elixir strings are UTF-8 encoded binaries, Erlang strings use double quotes but are Charlists

* variables get uppercase in Erlang; `x1` in Elixir  would be `X1` if written in Erlang

---
