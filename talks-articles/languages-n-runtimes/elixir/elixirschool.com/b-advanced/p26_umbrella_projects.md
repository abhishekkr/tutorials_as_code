
## Umbrella Projects

* `mix` build tool allows splitting codebase into multiple apps

* to create an umbrella project, need to call as `mix new many_things --umbrella` with `--umbrella` flag

> above creates 2 important directories `./apps/` (for child projects) and `./config/` (for umbrella config)


### Child Projects

* can create normal applications into `many_things/apps/` with `mix new thing_1`, `mix new thing_2`

* now in umbrella project root all typical mix commands like compile can be run for all

* individual tasks can still be run in specific app directories


### IEx

* `iex -S mix` in umbrella dir let's you interact with all projects

---
