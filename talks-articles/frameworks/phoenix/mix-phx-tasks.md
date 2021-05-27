
## Mix Tasks for Phoenix

> [doc](https://hexdocs.pm/phoenix/Mix.Tasks.Phx.html)

* `mix compile.phoenix` compiles Phoenix source files that support code reloading

* `mix local.phx` updates Phoenix project generator locally

* `mix phx` prints phoenix tasks and info

* `mix phx.digest ..` digests & compress static files

* `mix phx.digest.clean` removes old version of compiled assets

* `mix phx.gen.cert ..` gens self-signed certs for HTTPS testing

* `mix phx.gen.channel ..` gens a Phoenix Channel

* `mix phx.gen.context ..` gens context with functions around an Ecto schema

* `mix phx.gen.embedded ..` gens an Embedded Ecto schema for casting/validating data outside DB

* `mix phx.gen.html ..` gens controllers, views & context for HTML resource

* `mix phx.gen.json ..` gens controllers, views & context for a JSON resource

* `mix phx.gen.live ..` gens liveview, templates & context for a resource

* `mix phx.gen.presence ..name..` generates a Presence tracker

* `mix phx.gen.schema <module-name> <table-name> <attr-name>:<attr-type> ...` generates ecto schema & migration

* `mix phx.gen.secret [length]` generates a secret and prints (default length is 64; minimum allowed in 32)

* `mix phx.new <appname>` to generate new Phoenix App

* `mix phx.new.ecto <appname>` to generate new Phoenix Ecto (non-web) App

* `mix phx.new.web <appname>` to generate new Phoenix Web (non-ecto) App

* `mix phx.routes [<custom-router-module-name>]` prints all routes of default/given router

* `mix phx.server` run phoenix app server

---
