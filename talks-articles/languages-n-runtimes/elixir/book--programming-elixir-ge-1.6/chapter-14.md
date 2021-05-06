
## Chapter.14 Tooling

> after `OTP applications` have been explored, Elixir release manager (to release while app keeps running) will be explored

### Debugging with IEx

> code at [chapter-14-debug.ex](./chapter-14-debug.ex)

* this REPL can also be used as debugger at Breakpoints; can add calls from code or from inside IEx

* let's take buggy code to decode data part of MIDI header

* `Buggy` fails to decode for no clause matching

#### Inject Breakpoint

* Inject breakpoint using `require IEx; IEx.pry` as in `Buggy.pry_parse_header` module; we `require` since `pry` is a macro

> if we run `elixir <above exs>`; it says can't pry because IEx shell ain't running; so `iex <above exs>` which asks to allow (default yes) pry-ing

* **`binding`** shows local variables, which tells 'division' is '120' which doesn't match either decode clause because `decode` expects a binary not integer can fix it changing `division::integer-16` to `division::bits-16` in `fixed_parse_header` def

#### Setting Breakpoint with Break

* only capable to break at public functions

* can use `require IEx ; break! <Module.func>` in IEx can place a breakpoint at any public function

> if `.beam` file of module ain't available; `break!` fails
>
> so `elixirc <file>.ex` before, then `iex` and then `break!` it

* can check help on `IEx.break/4` and more to be more skilled


### Testing

> using ExUnit in Chapter.13 only scratched the surface

#### Testing the Comments

> utilizing `gitex` project created earlier

* let's add `Gitex.version` returning version from MixProject config; now if doc gets an IEx example showing `0.1.0` instead of `"0.1.0"` it will fail doctest for syntax error

> because we have `doctest Gitex` in `gitex_test.exs`

* now beware of copy/pasting examples as if doc is running some other function name in doc that is what will be used for the doctest irrespective for whatever the defined function below doc is

* important lines are `iex> ..foo-call..` & following non-blank line with output

* as the `Gitex.hello/1` def added, new tests added to doc show contiguous consecutive `iex>` share scope for following output line

#### Structuring Tests

> sample usage in [GitexGithubIssuesTest](./gitex/test/gitex/github_issues_test.exs)

* use `describe` to group bunch of realted tests (like varying scenarios for same function); as in `GitexCliTest`

* duplication occurs to create data, use `setup` feature to move this code into a single place which is available under `fixture.<data>` here as that is chosen during test def

```
setup do
  [ files: ["this", "that"],
    operation: "r",
    lines: 10
  ]
end

...
...
test "some blah thing", fixture do
  assert length(fixture.files) == 2
end
```

> can also move answers here, so changing tests in future just happens at one place if it involves output behavior

* `setup` invoked auto before each run; similar to it but once before all is `setup_all`

> can define callbacks using `on_exit` for things like cleanup after tests and such

* instead of passing the block to `setup`, can pass the function name as an atom

#### Property-Based Testing

* there are multiple libraries enabling it, we'll use StreamData (it's all generators are Elixir enumerables, it is stateless)

* very helpful at finding boundary conditions, which are expected data-sets miss due to problem focused ideation

* is to not rely on deciding assertions ahead of time; but utilize properties of function you are testing to predict and generate inputs and outputs.. so capability of function gets tested instead of our idea about its capability

> like `XString.upcase/1` input and output will be string of same length; output will have uppercase version of all lowercase characters in input and everything else will remain unchanged

> updated `2nd test in GitexCliTest` to a `property`; need to add `use ExUnitProperties` in module for properties to be available and `{:stream_data, "~> 0.5.0"}` to `mix.exs`
>
> * here `check all ..<generators>.. do ..usage.. end` gets used where things like `integer()` are generators
>
> * this iterates for a set of generated values, 100 by default

* there are many generators available for different types like `real()`, `list_of(integer())`, `member_of(any_existing_list)`, `binary()`

* we can also tweak generators to skip like

```
check all l <- list_of(integer()) |> nonempty do ## skips []
  asert ...
end

check all l <- list_of(integer(), min_length: 1) do ## forces generation of non-empty
  asert ...
end
```

* StreamData could be used in non-test code as well

#### Test Coverage

* we'll try `{:excoveralls, "~> 0.14.0"}` in `mix.exs` to check `Gitex`'s test coverage

* integrate coverall command into `MixProject.project` and configure to run in test environment only by adding `test_coverage` & `preferred_cli_env` values

* running `mix coveralls` shows the coverage output; like

```
% mix coveralls
Compiling 3 files (.ex)
Generated gitex app
..
01:11:30.940 [error] Github Fetch failed with HTTP 404
................

Finished in 0.1 seconds
9 doctests, 1 property, 8 tests, 0 failures

Randomized with seed 806368
----------------
COV    FILE                                        LINES RELEVANT   MISSED
100.0% lib/gitex.ex                                   35        2        0
 46.7% lib/gitex/cli.ex                               81       15        8
100.0% lib/gitex/github_issues.ex                     28        5        0
[TOTAL]  63.6%
----------------
```

* `mix coveralls.detail` produce detailed code-level output and `mix coveralls.html` generates `cover/excoveralls.html`


### Code Dependencies

* mix analyzes dependencies between source files, only recompiles a file when changed or its dependency changed

* can access dependency info via `mix xref <subcommand>`, can do `mix help xref` for details of its powers like

> * `mix xref unreachable` listing unknown `module.function/arity` calls made; can suffix `--abort-if-any` for it to fail
>
> * `mix xref callers <Module|Module.func|Module.fun/arity>` lists callers to provided call scope
>
> * `mix xref graph` shows dep tree for app; suffix with `--format dot` produces something viewed by `dot -Grankdir=LR -Epenwidth=2 -Ecolor=#a0a0a0 -Tpng name_of.dot -o name_of.png`
>
> can also do format for pretty, plain, stats
>
> * `mix xref graph --sink some/file.ex --only-nodes` get all files depending on `some/file.ex`


### Server Monitoring

* one of easiest is baked in, at IEx run `iex> :observer.start()` to open a GUI console with insights into Erlang VM for detailed reports

> gives basic system info, dynamic charts of load, info and contents of Erlang ETS Tables, running process & applications, memory allocation; alongwith tracing of function calls

* for app-level monitoring try things like `Elixometer` by _Pinterest_


### Source-Code Formatiing

* `mix format` can format single files, dir trees & whole projects; replaces the file it touches

* `mix help format` for more info on usage


### Inevitably, There's More

* inherits a lots from Erlang; and many just for Elixir... keep up

---
