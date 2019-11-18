
## IEx Helpers

* `<tab>` can be used for AutoComplete help

* Elixir code within `.iex.exs` config file in `cwd` (if not, then home dir) will be made available at REPL

* Can add code like following and run `IExHelpers.type?("string")` at IEx

```
defmodule IExHelpers do
  def type?(term) when is_nil(term), do: "Type: Nil"
  def type?(term) when is_binary(term), do: "Type: Binary"
  def type?(term) when is_boolean(term), do: "Type: Boolean"
  def type?(term) when is_atom(term), do: "Type: Atom"
  def type?(term) when is_number(term), do: "Type: Number"
  def type?(_term), do: "Type: Unknown"
end
```

* `h` gives support for doc of any code, like `h Enum`

* `i` prints information about given data type, like `i Map`

* `r` to recompile a particular module, like `r MyModule`

* `t` tells about Types in a given module, like `t Map`

---
