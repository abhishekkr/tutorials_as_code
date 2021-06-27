
## Chapter.11 Observer and Umbrellas

> [extracted umbrella app](https://github.com/abhishekkr/videologue_umbrella)
>
> * channels & MVC components currently support user interfaces & directly communicate with biz backend
>
> * here these will refactored into child apps


### Introspecting Applications with Observer

> running Videologue starts multiple Applications alongside; Supervisors start/stop apps as unit using Supervision Tree details

* can start `:observer.start()` at `iex -S mix`; it allows understanding running processes for app

* in processes tab, ordering process by MsgQueue size might allow see the bottleneck

* Applications tab show all applications & its Supervision Tree; it will miss the server since `iex -S mix phx.server` wasn't started

> processes can be killed from here to check

* moving to umbrella, can keep Web & Backend separate as separate Applications but in same repository without version issues


### Using Umbrellas

* each Umbrella project has a parent dir which contains `apps` dir & share config for projects

* can use `mix new videologue --umbrella`; but let's `mix phx.new videologue --umbrella` as it will automate some web-based path & config generation

> let's press `n` for dependency install; as we'll  copy existing elixir and js code; we have `videologue_umbrella` project now
>
> * using `phx.new ... --umbrella` auto creates `videologue_umbrella/apps/{videologue,videologue_web}`

* new created `apps` configuration would be like

```some_umbrella/apps/some_web/mix.exs
def project do
  [
    app: :some_web,
    version: "0.1.0",
    build_path: "../../_build",
    config_path: "../../config/config.exs",
    deps_path: "../../deps",
    lockfile: "../../mix.lock",
    elixir: "~> 1.7",
    elixirc_paths: elixirc_paths(Mix.env()),
    compilers: [:phoenix, :gettext] ++ Mix.compilers(),
    start_permanent: Mix.env() == :prod,
    aliases: aliases(),
    deps: deps()
  ]
end

...

defp deps do
  [
    {:phoenix, "~> 1.5.9"},
    ...
    {:videologue, in_umbrella: true},
    ...
  ]
end
```

> all children apps share same config & dependency; are isolate but ran on same VM


### Extracting Videlogue and VideologueWeb

* copy over content for `videologue` app

```
cp -R videologue/lib/videologue videologue_umbrella/apps/videologue/lib
cp videologue/lib/videologue.ex videologue_umbrella/apps/videologue/lib
cp -R videologue/test/videologue videologue_umbrella/apps/videologue/test
cp -R videologue/priv/repo videologue_umbrella/apps/videologue/priv
cp videologue/test/support/data_case.ex videologue_umbrella/apps/videologue/test/support
cp videologue/test/test_helper.exs videologue_umbrella/apps/videologue/test
```

> update [videologue\_umbrella/videologue/application](videologue_umbrella/apps/videologue/lib/application.ex) to have only `Repo` under children

```
    children = [
      Videologue.Repo,
    ]
```

* copy content for `videologue_web` app

```
cp -R videologue/lib/videologue_web videologue_umbrella/apps/videologue_web/lib
cp -R videologue/lib/videologue_web.ex videologue_umbrella/apps/videologue_web/lib
cp -R videologue/test/videologue_web videologue_umbrella/apps/videologue_web/test
cp -R videologue/test/support/conn_case.ex videologue_umbrella/apps/videologue_web/test/support
```

> * update `endpoint.ex` to have `:videologue_web` as `otp_app` attrib & `from` under `Plug.Static`
>
> * also update `otp_app` under `presence.ex` & add `Videologue.Presence` under Applications's children tree & also `{Phoenix.PubSub, name: Videologue.PubSub}`

* copy assets

```
cp -R videologue/assets/js videologue_umbrella/apps/videologue_web/assets/js
cp -R videologue/assets/css videologue_umbrella/apps/videologue_web/assets/css
```

> check dep paths under `apps/videologue_web/assets/pacakge.json` to point for root umbrella
>
> * add `config :videologue, phoenix_token_salt: ..` to configs 


#### Running Tests

```
mix deps.get
push apps/videologue_web/assets ; npm install ; popd
```

* add `config :pbkdf2_elixir, :rounds, 1` to test config


### Wrapping Up

> [Chapter-12 OTP](./chapter-12.md

---
