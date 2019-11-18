
## Documentation

> sample code at [p11_documentation.exs](./p11_documentation.exs)

### Annotation

* doc treated as first-class citizen; Elixir provide many attributes to annotate a codebase

> * `#` for inline doc
>
> * `@moduledoc` for module-level doc
>
> * `@doc` for function-level doc

* to check doc

> if just compiled at `iex` in memory, no `*.beam` file with bytecode gets created

```
iex> c("p11_documentation.exs")
Hey Jane.
[Speech.Greet]
iex> h Speech.Greet
Speech.Greet was not compiled with docs
```

> for `iex` help to work on custom module

```
iex> c("p11_documentation.exs", ".")
[Speech.Greet]

iex> h Speech.Greet
                                  Speech.Greet

Provides greeting ...


iex> h Speech.Greet.hey

                                 def hey(name)

  @spec hey(String.t()) :: String.t()

Returns hey ...
```

> `@spec` annotation is used to statically analyze code, [learn more here](https://elixirschool.com/en/lessons/advanced/typespec)


### ExDoc

> official Elixir project producing HTML and online doc

#### Installing

```
$ mix new greet_em_all
...

$ cd greet_em_all
```

* update `def deps` under `mix.exs` for `{:earmark, "~> 1.2", only: :dev}, {:ex_doc, "~> 0.19", only: :doc}`

> here Earmark is a markdown parser, utilized by ExDoc to turn doc into nice HTML; these can be changed to Pandoc, Cmark or others

#### Generating Documentation

```
mix deps.get
mix docs
```

> above generates `doc/` directory


### Best Practice

* [some of best practices can be seen here](https://github.com/niftyn8/elixir_style_guide)

* always document a module using `@moduledoc """..."""`; on skipping remember to put `@moduledoc false`

* use backticks and provide arity when refering to functions within module doc

* can use Markdown within doc, always give a blank line after module doc

* include some code examples in doc, allows to generate automatic tests from the code eamples found in module, function or macro with `ExUnit.DocTest`

---
