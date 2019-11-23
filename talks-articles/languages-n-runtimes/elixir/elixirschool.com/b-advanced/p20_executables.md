
## Executables

> can use `escript` to build executable, which can be run anywhere with Erlang installed


### Getting Started

* need to implement a `main/1` function in code

```
defmodule MyApp.CLI do
  def main(args \\ []) do
    ## call whatever need to be run
  end
end
```

* and update Mixfile as following with `project/0`

```
defmodule MyApp.Mixfile do
  def project do
    [app: :my_app, version: "0.0.1", escript: escript()]
  end

  defp escript do
    [main_module: MyApp.CLI]
  end
end
```


### Parsing Args

* can use `OptionParser.parse/2` with `:switches` option, to parse cli args

```
defmodule MyApp.CLI do
  def main(args \\ []) do
    args |> parse_args() |> response() |> IO.puts()
  end

  defp parse_args(args) do
    {opts, word, _} = args
                       |> OptionParser.parse_args(switches: [upcase: boolean])

    {opts, List.to_string(word)}
  end

  defp response({opts, word}) do
    if opts[:upcase], do: String.upcase(word), else: word
  end
end
```


### Building

* `mix escript.build` can build executable once above steps are done

* run like `./my_app --upcase hey` or `./my_app hello`

---
