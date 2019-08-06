
## Mix

> source: [hexdocs.pm](https://hexdocs.pm/mix/Mix.html), `v1.9.1`

* is a build tool with tasks for creating, compiling, and testing elixir projects; managing dependencies and more

### Mix.Project

* project can be defined using `Mix.Project` in a module, usually in file `mix.exs`

```
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app,
      version: "1.0.0"
    ]
  end
end
```

> on defining project, bunch of mix commands become available as `mix compile`, `mix test` and `mix run`

* each task has options and sometimes specific conf to be defined in `project/0` func; can use `mix help <func-name>` to show help for a task

* best way to create project `mix new my_project`


### Mix.Task

* let you extend `mix` with custom tasks; as `mix hello` for following

```
defmodule Mix.Tasks.Hello do
  use Mix.Task

  def run(_) do
    Mix.shell().info("can put run flow here")
  end
end
```


### Dependencies

* integrate dependencies management nicely with [hex](https://hex.pm/) package manager

* add `:deps` key to manage dependencies

```
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [ app: :my_app, version: "1.0.0", deps: deps() ]
  end

  defp deps do
    [
      {:ecto, "~> 2.0"},
      {:plug, github: "elixir-lang/plug"}
    ]
  end
end
```


### Environments

* mix provides 3 environments; `:dev` (default), `:test` (for `mix test`), `:prod` (for dependencies)

* env can be changed with `MIX_ENV` env var; as `MIX_ENV=prod mix run server.exs`

* can specify env-only dependencies like `{:dep_name, "~> 1.0", only: :dev}`

* environment available via `Mix.env/0`


### Targets

* useful to compile for different architectures with contextual dependencies; experimental

* default target is `:host`, can be set via env `MIX_TARGET` and read via `Mix.target/0`


### Aliases

* are shortcuts for tasks, used as `mix c` alias for `compile` or `mix hello` for `Mix.Tasks.Hello`

```
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [ app: :my_app, version: "1.0.0", aliases: aliases() ]
  end

  defp aliases do
    [ c: "compile", hello: &hello/1 ]
  end

  defp hello(_) do ## private as defined with defp
    Mix.shell().info("Hello world")
  end
end
```

* can also be a list specifying multiple tasks as `[all: [&hello/1, "deps.get --only #{Mix.env()}", "compile"]]`

* can also have same task name augmented as `[compile: ["deps.get --only #{Mix.env()}", "compile"]]`

* aliases defined in dependencies are not accessible from current project

* can orchestrate elixir scripts with `mix run` and shell commands with `mix cmd`

```
# priv/hello.exs
IO.puts("Hello")

# priv/world.sh
echo "World!"

# mix.exs
defp aliases do
  [ some_alias: ["hex.info", "run priv/hello1.exs", "cmd priv/world.sh"] ]
end
```

* mix tasks run only once unless re-enabled

```
another_alias: [
  "format --check-formatted priv/hello1.exs",
  "cmd priv/world.sh",
  fn _ -> Mix.Task.reenable("format") end,
  "format --check-formatted priv/hello2.exs"
]
```

* automatically re-enabled tasks are `mix cmd`, `mix do`, `mix loadconfig`, `mix profile.cprof`, `mix profile.eprof`, `mix profile.fprof`, `mix run` and `mix xref`


### Environment Variables

* `MIX_ARCHIVES`, dir to install archives

* `MIX_BUILD_PATH`, project build-path config

* `MIX_DEBUG`, outputs debug info about each task before running it

* `MIX_ENV`, specifies which env should be used

* `MIX_TARGET`, specifies which target to used

* `MIX_EXS`, changes full path of `mix.exs`

* `MIX_HOME`, path to Mix's home dir, stores config files and scripts used

* `MIX_PATH`, appends extra code path

* `MIX_QUIET`, doesn't print info to terminal

* `MIX_REBAR`, override path to rebar than one installed by mix

* `MIX_REBAR3`, override path to rebar3 than mix install

* `XDG_DATA_HOME` and `XDG_CONFIG_HOME` can be used too


### Summary

* `compilers()`, returns the default compilers used by Mix

* `debug(debug)`, sets Mix debug mode

* `debug?()`, returns true if Mix is in debug mode, false otherwise

* `env()`, returns the current Mix environment

* `env(env)`, changes the current Mix environment to env

* `raise(message)`, raises a Mix error that is nicely formatted

* `shell()`, returns the current shell

* `shell(shell)`, sets the current shell

* `target()`, returns the Mix target

* `target(target)`, changes the current Mix target to target


---
