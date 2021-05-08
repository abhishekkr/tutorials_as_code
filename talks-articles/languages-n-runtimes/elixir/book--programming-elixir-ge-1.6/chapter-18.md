
## Chapter.18 OTP: Supervisors

> project [supseq18](./supseq18) has been created as a sample following this chapter

* traditional applications are taken down by one error and end up losing several happy path requests

* what if among several processes of application, one crashes but the other carry on; Supervisors perform this monitoring and restarting


### Supervisors and Workers

* An Elixir Supervisor manages one or more processes; which could be Workers or yet another Supervisors.

* It's given a process list to monitor and action for its crash; avoid restart loops. It utilizes BEAM's process linking and monitoring for it.

> Can write Supervisors as separate modules; convention is to include them **inline**.

* Can create a project with Supervisor easily by `mix new --sup <project-name:-supseq18>`; which also creates a [application.ex](./supseq18/lib/supseq18/application.ex) under `lib/<project-name:-supseq18>` with boilerplate for `Supseq18.Application`

> * `Supseq18.Application.start/2` creates supervisor; we need to specify what need be supervised
>
> * we'll utilize Sequence server from previous chapter for [server.ex](./supseq18/lib/supseq18/server.ex) as `Supseq18.Server`
>
> * add `{Supseq18.Sever, []}` as an element to `children` list in `start/2` which will ensure for call `Supseq18.Server.start_link([])`

* now running project `iex -S mix` and just using `Supseq18.Server.pop` would work since starting our application triggers `Application.start` which runs `start_link` for all its children

* Currently it supports pushing `nil`; let's `raise` at that attempt. Now it gives an error message as below but keeps working irrespective as if Server never crashed but loses previous state

```
iex(2)> Supseq18.Server.push 10
:ok
iex(3)> Supseq18.Server.push nil
:ok
iex(4)>
01:38:55.971 [error] GenServer :supstack terminating
** (RuntimeError) You Have Been Bad!
    (supseq18) lib/supseq18/server.ex:12: Supseq18.Server.handle_cast/2
    (stdlib) gen_server.erl:637: :gen_server.try_dispatch/4
    (stdlib) gen_server.erl:711: :gen_server.handle_msg/6
    (stdlib) proc_lib.erl:249: :proc_lib.init_p_do_apply/3

nil
iex(5)> Supseq18.Server.pop
nil
iex(6)> Supseq18.Server.push 12
:ok
iex(7)> Supseq18.Server.pop
12
```

#### Managing Process State Across Restarts

* some servers are effectively stateless, can be restarted and just done with it

* our server holds a Stack of data (state); which needs to be stashed.. we can do it in a separate Process that can store & retrieve a value

> our stash process must be ROBUST to outlive the Server for which it does Stash at least
>
> * need to keep Stash process simple and isolated from failures of Stack Server
>
> implement a new GenServer for it as `Supseq18.Stash` under [stash.ex](./supseq18/lib/supseq18/stash.ex)

* `Supseq18.Stash` will be running alongside `Supseq18.Server`, now we need to decide what happens if one crashes

* Decision on crash is chosen by **Supervision Strategy**

> * (default) `:one_for_one`, if a server dies it gets restarted
>
> * `:one_for_all`, if a server dies then first all remaining are terminated & then restarted
>
> * `:rest_for_one` if a server dies, servers following it in list of children are terminated & then dying server followed by all their restarts


* We can update `opts` for `strategy: :rest_for_one`; add `Stash` entry above `Server` in `children` list

> explicitly showing dependency on Stash and restart Server if Stash crashes. Not necessary for our use-case though

* Update `Supseq18.Server` to use `Stash` and test again.

#### Simplifying the Stash

> `Exercise: OTP-Supervisors-2` TBD

> Sole job of stash module is to store a value. Agents (covered later) are perfect fit for this to simplify.


### Worker Restart Options

* can fine tune strategy at each worker level like `:restart` option in tuple passed to children OR in `use GenServer, restart: :transient` like pattern within Worker itself

> * `:permanent` make always restart when required as per Supervisor Strategy
>
> * `:temporary` make worker never restart, so no Strategy applied
>
> * `:transient` means Worker will be terminated sometime and should not be restarted then; but if dies abnormally apply the Supervision Strategy

#### A Little More Detail

* `children` list passed to Supervisor is list of `child_specs` that is a map describing which worker it is and how to deal with it

* can create a child specification map using `Supervisor.child_spec/2`; when you add `use GenServer` to a module Elixir defines a default `child_spec` in it and calls it to get the specification


### Supervisors Are the Heart of Reliability

* Supervisor have way more capability not discussed here

* different stratgies to terminate, dealing with termination, different ways of restarting

* Joe Armstrong says OTP have been used to build systems with `99.9999999%` reliability

> [detail hexdocs](https://hexdocs.pm/elixir/Supervisor.html)

* we'll use what we learned to build something next; then move to next OTP layer `Application`

---
