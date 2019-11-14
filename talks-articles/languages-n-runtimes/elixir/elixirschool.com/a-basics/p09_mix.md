
## mix

* with analogy to ruby, `mix` is bundler+rubygems+rake

### new projects

* `mix new <project_name>` create boilerplate code structure alongwith `mix.exs` with content like

```
defmodule Example.Mixfile do
  use Mix.Project

  def project do
    [
      app: :example,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    []
  end
end
```


### interactive

* to use `iex` within context of our application, can run `iex -S mix`


### compilation

* run `mix compile` in base dir, `_build` dir gets created for artifacts with `example.app` in this case


### managing dependencies

* to add/remove dependencies first add to `deps` list section in `mix.exs` with 2 required values of package name and version

```
def deps do
  [
    {:cowboy, "~> 1.0", only: [:dev, :test]},
    {:slime, "~> 0.14"}
  ]
end
```

* fetch required dependencies using `mix deps.get`


### environments

* out of the box configured to have 3 environments

> * `:dev`, default environment
>
> * `:test`, used by `mix test`
>
> * `:prod`, used when we ship app to production

* current env is accessible using `Mix.env`, environment can be set using system env var `MIX_ENV`

```
MIX_ENV=prod mix compile
```

---
