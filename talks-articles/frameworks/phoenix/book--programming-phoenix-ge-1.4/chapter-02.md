
# Part.I Building with Functional MVC

## Chapter.2 The Lay of the Land

### Simple Functions

> the beauty of Elixir language works seamlessly here

* classic layers of Phoenix for HTTP request would like `connection |> endpoint() |> router() |> pipelines() |> controller()`


### Installing your Dev Env

* `Erlang |> Elixir |> Phoenix`; installing Hex `mix local.hex`; Ecto can work with `PostgrSQL`

> * if Frontend is involved one need to mannage Assets; need `nodejs |> npm` for it.. Phoenix will use `webpackjs` to compile
>
> * `inotify` of linux for live reloading to work

* using Mix to install Phoenix

```
mix archive.install hex phx_new

mix phx.new -v

mix phx.new demo --live
```

* `mix phx.new.ecto just_api` and `mix phx.new.web just_frontend_nodb` options are also available

### Creating a Throaway Project

* create a project `mix phx.new ehlo` and `Y` to install dependencies

> if install dependencies fail here

```
pushd ehlo
mix deps.get  # for Phoenix deps
pushd assets
npm install && node node_modules/webpack/bin/webpack.js --mode development # for assets compilation
popd
mix deps.compile`# for phoenix deps
popd
```

* now cd to `ehlo`; `mix ecto.create` to after DB config at `config/dev.exs` for db model creation; `mix phx.server` to run at `:4000` by default or `iex -S mix phx.server` to run with REPL

> * in `config/dev.exs` for `MIX_ENV=dev`; DB configs belong under `config :ehlo, Ehlo.Repo, ..`
>
> * to change listening PORT or other server props goto, `config :ehlo, EhloWeb.Endpoint, ..`
>
> * visiting `/` gives Getting Started page


### Building a Feature

* for 1st feature let's just print a string at a specific URL

> the created `lib/ehlo_web` has dirs for `channels`, `controllers`, `templates/{layout,page}`, `views` alongwith files like `router.ex`, `endpoint.ex`, `gettext.ex`

* `router.ex` here contains block of requests; with pre-defined `get "/" ...` under scope `/`

* this scope shall handle for all paths starting `/`; add `get "ehlo", EhloController, :user` to this scope

> * visiting page now will error for `EhloWeb.EhloController.init/1 is undefined`

* let's add controller as [ehlo\_controller.ex](ehlo/lib/ehlo_web/controllers/ehlo_controller.ex)with `user` action defined

> * visiting page now error for `EhloWeb.EhloView.render/2 is undefined`

* adding [user.html.eex](ehlo/lib/ehlo_web/templates/ehlo/user.html.eex)

> now `localhost:4000/ehlo` shall display new page; `*.eex` files here are templates

* add dynamix data from URL to Routes like `get "/ehlo/:name", EhloController, :user`, to pass a username which will be available as `def user(conn, %{"name" => name})..` in Controller which will pass it to `render`

> * use pat-match function def for `user` for `/ehlo/:name` above `/ehlo`
>
> * external parameter key is a string `"name"` not atom as a convention for Phoenix to not trust conversion for external data
>
> * then adding an if condition to `user.html.eex` checking if `assigns` Map passed with parameters to Template has a key `name` or not to pick required value does the page render with user name at `/ehlo` and `/ehlo/alice` correctly

* `<%= %>` surround Elixir code blocks in EEx; Assigns enable parameter passed available


### Going Deeper: The Request Pipeline

* Phoenix encourages breaking bigger functions into smaller ones; then provide a place to explicitly register each cog in a way easy to understand and replace

> all these cogs tied together with `Plug` library

* `Plug` lib act as a spec for web apps; each Plug consumes and produce a common data-struct `Plug.Conn` representing entirety for a request... Phoenix just calling one Plug after another

> Response is just one more action on `connection`
>
> Plugs are functions and Web-apps are pipelines of Plugs

* phoenix file structure

> * config goes at `./ehlo/config`
>
> * `./ehlo/assets` get JS and CSS
>
> * `./ehlo/lib/ehlo` gets supervision trees, long-running processes, app business logic
>
> * web related code for routes, controllers, views (templates), channels go to `./ehlo/lib/ehlo_web`
>
> * migrations go to `./ehlo/priv/repo` and some boilerplate test setup is at `./ehlo/test`
>
> * config structure and env relation is similar to any other Mix project

* `EhloWeb.Endpoint` (a Plug made of multiple other Plugs) are the chain of functions at the beginning of each request

> a Connection begins with an Endpoint, finishes at Controller

* Phoenix doesn't limit Endpoint count

> * so an app can have main app Endpoint running at Port `80,443` and admin Endpoint running at a special port like `4443`
>
> * we can also split these Endpoints into separate applications still run side-by-side (using Umbrella Projects)

* Router by default has 2 pipelines, Browser for HTML and API for JSON. For matched scope it calls Pipeline mentioned and that passes to Controller


### Wrapping Up

> [chapter-03](./chapter-03.md)

---
