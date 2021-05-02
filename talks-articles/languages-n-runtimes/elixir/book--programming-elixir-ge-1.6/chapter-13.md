
## Chapter.13 Organizing a Project

> code example project is at [gitex](./gitex)

### The Project: Fetch issues from Github

* Github Public API `GET https://api.github.com/repos/$user/$project/issue` provides JSON list of issues. Reformat it, sort, filter out oldest `n`, presenting result as table.

> input will be user-name, project-name and optional count; so need basic cli parsing


### Step.1 Use Mix to create new Project

* `mix help` to get list of capabilities; `mix help deps` like run to help on an option

* `mix new gitex` to create new project structure; can `cd gitex` & run `mix test`


### Transformation: Parse Cmd Line

* don't couple cli options handling in main body, but a separate module by convention called `<ProjectName>.CLI` so `Gitex.CLI`

* by convention `Gitex.CLI` shall be defined in [cli.ex](gitex/lib/gitex/cli.ex) under `lib/gitex` dir

> will use available option-parsing library `OptionParser.parse/3`

* define `Gitex.CLI.run/1` and `Gitex.CLI.parse_args/1` to parse passes argv list


### Write Basic Tests

* write test to verify `-h|--help` and `user-handle project-name [count]` of `Gitex.CLI.run/1` at [test/gitex/cli_test.exs](gitex/test/gitex/cli_test.exs)


### Refactor: Big Func Alert

* `Gitex.CLI` to be refactored from Case usage to Private function pattern matching under `...2nd_iteration` comment


### Transformation: Fetch from Github

* `Gitex.CLI.process/1` to process required cli option

* can run a specific func as `% mix run -e 'Gitex.CLI.run(["-h"])'`


### Step.2 Use Libraries

* add a library `finch` to `mix.exs`; install via `mix deps.get`

* using [docs](https://hexdocs.pm/httpoison) add `HTTPoison.get/2` for Github Issues; returning `{:ok, body}` or `{:error, body}`

* `HTTPoison.start` gets called 'cuz it runs as a separate app outside main process

> older versions, had to add HTTPoison to list of applications in start in `mix.exs`; no longer necessary
>
> it being listed as dependency, Mix will automatically start it

* Code that would be library in other Programming Languages, is a subapplication in Elixir. Think of them as services.

> can test out functionas at `iex -S mix`
>
> have used `Mock` library to return fakes for testing of `fetch/2`, for better testing use VCR


### Transformation: Convert Response

* use another library `Poison.Parser.parse!/2` to parse JSON response received from Github Issues

* make issues URL configurable by adding a `config/config.exs` at root, also have `<env>.exs` files here to override any config required

> * should start with line `use Mix.Config`
>
> * then configurable attributes `config :gitex, github_api_url: "https://api.github.com"`
>
> * with multiple lines, the key-val environment pairs accumulate, later duplicates overriding earlier one
>
> * add `import_config "#{Mix.env}.exs"` to import env specific overrides

* `Application.get_env/2` returns value from environment

* can override config file name with `--config <file>` elixir option


### Transformation: Sort Data

* sort the data on `created_at` field in reverse chronological order

> also add tests for it


### Take First n Items

* resist writing your own, use `Enum.take/2`


### Transformation: Format the Table

* add a `Gitex.CLI.print_table/1` with formatted print for pattern-match extracted fields for each element

* use `capture_io` from `ExUnit.CaptureIO` to test stdout stuff


### Step.3 Make a CLI Executable

> Mix can package code with deps into a single file. Using Erlang's `escript` util, one can run pre-compiled programs stored as Zip archive.

* add `escript: [main_module: Gitex.CLI]` to `Gitex.MixProject.project` list config in `mix.exs` to be able to use Escript

* add `main` to `Gitex.CLI`; we can just rename `run` to `main`

* pacakge via `mix escript.build` & can run on any erlang installed machine as `./gitex elixir-lang elixir 10`

* can list all files bundled as `unzip -l ./gitex`


### Step.4 Add Some Logging

* `:logger` is already added to `extra_applications` config in MixProject

* let's add `config :logger, compile_time_purge_level: :info` in `config/config.exs` to disable level below which logging wouldn't be even compiled

* `Logger.configure` to config minimum level logging, or `config :logger, level: :warn`

* `require Logger` and `Logger.debug|info|warn|error` taking a string or 0-arity function

* add `dev.exs` with info log-level, `production.exs` with warn log-level; building escript with `MIX_ENV=production` or other mix commands will use production config

* add few info and error logs around HTTP Client operations


### Step.5 Create Project Documentation

* elixir has `ExDoc` like JavaDoc in Java and RDoc in Ruby

* add `ex_doc` lib to `mix.exs`; output formatter `earmark` is a good lib to add as well

> can also use `ExUnit.DocTest` to extract test-cases from documentation, generate test-cases & invoke tests using [doc](https://hexdocs.pm/ex_unit/ExUnit.DocTest.html)

* add `ex_doc` config to `Gitex.Mixproject.project` return list

* run `mix docs` to generate documentation, it's gitignore-d


### Coding by Transforming Data

> lots of small functions transforming data is a slick way to code

---
