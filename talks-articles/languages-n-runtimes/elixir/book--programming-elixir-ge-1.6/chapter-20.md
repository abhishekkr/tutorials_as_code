
## Chapter.20 OTP: Applications

### This is not your Father's Application

* for OTP, an `Application` is a bundle of code that comes with a descriptor which defines its dependencies, global names it registers, more

* OTP Application is similar to a DLL (component or service) than a traditional app

> HTTPoison is an independent application automatically loaded by Mix; which in turn started couple of other application (SSL, Hackney)

* Applications are components but some applications are at top of tree & meant to be run directly


### The Application Specification File

* `<application name>.app` contains app spec; created automatically by Mix using info from `mix.exs`

* this file is used to load, all OTP func might not be needed


### Turning Our Sequence Program into an OTP Application

* example project [distill20](./distill20)'s `mix.exs` file shows top-level module is `Distill20`

```
def application do
  [ extra_applications: [:logger], mod: {Distill20.Application, []} ]
end
```

* OTP assumes `Distill20` implements a `start` function,  to which it passes empty list; following change makes `101` being sent to `start`

```
  [ extra_applications: [:logger], mod: {Distill20.Application, 101} ]
```

* `mod: {}` option helps configure app's main entry point module for OTP

* `registered: [Distill20.Server]` option can be added to `application` list alongside `mod: {}`; registered can be used to ensure each name is unique across all loaded applications in a node or clusters

* now `mix compile` compiles app and updates [distill20.app](./distill20/_build/dev/lib/distill20/ebin/distill20.app) at `_build/dev/lib/distill20/ebin`

> it contains some info from `project` & `application` in `mix.exs`; Mix auto adds list of names of all compiled modules and a list of apps our apps depends upon

* alongside `mod: {}`, we can pass `env: [var: :val]` to allow `Application.get_env(:distill20, :var)` pull 


### Supervision is the basis of Reliability

> *WORD*


### Releasing Your Code

* Release is a bundle that contains a particular version of Application (its dep, config & any metadata required)

* Deploymeny a way of getting Release in an Environment

* Hot Upgrade deploy-ing release of a currently running application while it keeps running (not looked in detail here)


### Distillery - The Elixir Release Manager

> [hexdocs for distillery](https://hexdocs.pm/distillery/introduction/installation.html)

* `Distillery` is a package to make release easy (even to deploy to phone switches keeping uptime)

* Code (version in `mix.exs`) and Data (type of state each server holds changes, version is managed using `@vsn "id"` below `use GenServer` directive) both are versioned independently

* first add `{:distillery, "~> 1.5", runtime: false},` to deps in `mix.exs`; then `mix do deps.get, deps.compile`

* `mix distillery.init` creates example Distillery config at `rel/config.exs` to be reviewed/edited

* `MIX_ENV=prod mix distillery.release` packages app into `_build/dev/rel` as `distill20.tar.gz`

```
## MIX_ENV for mix configs, in case need use different... default distillery env used is dev
$ MIX_ENV=prod mix distillery.release

==> Assembling release..
==> Building release distill20:0.1.0 using environment prod
==> Including ERTS 10.7.2.9 from /usr/lib64/erlang/erts-10.7.2.9
==> Packaging release..
Release successfully built!
To start the release you have built, you can use one of the following tasks:

    # start a shell, like 'iex -S mix'
    > _build/prod/rel/distill20/bin/distill20 console

    # start in the foreground, like 'mix run --no-halt'
    > _build/prod/rel/distill20/bin/distill20 foreground

    # start in the background, must be stopped with the 'stop' command
    > _build/prod/rel/distill20/bin/distill20 start

If you started a release elsewhere, and wish to connect to it:

    # connects a local shell to the running node
    > _build/prod/rel/distill20/bin/distill20 remote_console

    # connects directly to the running node's console
    > _build/prod/rel/distill20/bin/distill20 attach

For a complete listing of commands and their use:

    > _build/prod/rel/distill20/bin/distill20 help
```

#### Deploy & Run the App

* copy it over to host, extract the tarball say at `/app-releases` then scripts in `/app-releases/bin` enables running

* things like `ssh -t me@myhost /app-releases/bin/distill20 console` gets you remote IEx

* let's leave a session running using `distill20 console` & try hot code reloading

> * update some bheavior; update `@vsn` if server state gets updated else just update app version in `mix.exs`
>
> * after testing once reeady to release; running just `MIX_ENV=prod mix distillery.release` creates whole new releasable app
>
> * alternatively one can deploy an upgrade release, able to upgrade running app helping achieve high uptime

* once running, can connect with release using `bin/<appname> remote_console`

* if started using `start`, use `stop` to stop; if `foreground` to start, use `CTRL+C` or `kill -s PID`


#### Hot Swap Upgrade

> **With Great Power Comes Great Responsibility**; to understand what 
>
> flow for Distillery to use this have changed a bit since book, updated notes as of May/2021 from [doc](https://hexdocs.pm/distillery/guides/upgrades_and_downgrades.html) are here

* Distillery makes hot upgrade simple by generating `appups`; which Release Managers *must* check and make adjustments to, checking if anything is missed or confused. Alongwith implementing `code_change` in processes you want upgraded.

> one can always reboot an app and get back to a good state; also do a rolling release unless your app is critical to avoid lack of know-how impact feature release

* hot upgrades & `include_erts: false` can't be used in conjunction

> * need to set `include_erts: false` or `include_erts: /path/to/cross/compiled/erts` if build machine got different OS/Architecture than target machine
>
> * when not including ERTS, ensure Erlang with required libraries is present on target machine

* hot upgrades/downgrades mutate release directory; never test it in `_build`

* `rel/config.exs` say `default_environment: Mix.env()`; so both can be identified by just one build time conf

* using `MIX_ENV=prod mix distillery.release --upgrade`; this creates `.appup` file which helps upgrade

> * this new version dir available tarball to be copied to host under  `/app-releases/releases/0.2.3` if new version is `0.2.3`
>
> * run `/app-releases/bin/distill20 upgrade 0.2.3` which looks for tarball under `../releases` and installs
>
> * now updated feature shall be available over earlier console left running; without restart

* Erlang can run 2 versions of a module at same time; currently executing code will keep using older version

* If new release is a disaster, can always downgrade using `/app-releases/bin/distill20 downgrade 0.2.2` if earlier stable version is `0.2.2`

> can't downgrade to an arbitrary version, need to downgrade in the same order as of upgrade

* When building upgrade releases; Distillery ensures all modified apps have Appups generated for them

```
MIX_ENV=prod mix distillery.gen.appup
```

> * Appups tell `:systools` on how to generate `relup` file
>
> * `relup` contains low-level instructions for release handler; how to load/remove/change modules in system


#### Migrating Server State

* let's update state of Stack to add a TTL for stack, after which it's auto reset

* since state format updates in this, we bump up `@vsn`

* since state format changes, if we restart it loses old state; we can copy the state over with `code_change/3` function like

```
def code_change("0", old_state = current_stack, _extra) do
  new_state = %{stack: current_stack, ttl: 1_000_000_000}
  Logger.info("Changing Stack vsn from 0 to 1")
  Logger.info(inspect old_state)
  Logger.info(inspect new_state)
  {:ok, new_state}
end
```

> Callback takes 3 params; old version number, old state and additional parameter; returns `{:ok, NEW_STATE}`

* now bump up app-version as well alongwith state `@vsn`; create an upgrade release similar to before & deploy same way as well

> we'll notice `code_change/3` logs in console window ensuring state migration



### OTP is Big - Unbelievably Big

* here only OTP basics get covered; advanced uses may include hot code-swapping, distributed failover, automated scaling, and more

---

* Package [edeliver](https://github.com/edeliver/edeliver) enables build, deploy & hot upgrade Erlang/Elixir apps using SSH/SCP.

---
