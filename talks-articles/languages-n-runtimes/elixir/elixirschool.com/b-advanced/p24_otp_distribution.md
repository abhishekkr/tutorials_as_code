
## OTP Distribution

> [sample code](p24_otp_distribution.exs), sample app created at [p24_chat](./p24_chat)

* can run app on different nodes across multiple hosts, which can communicate via existing mechanism

### Communication between nodes

* Every erlang runtime system behaves as node, can have a named node even using `iex --sname me@mylocal`; using it can have multiple nodes locally with different names


### Communicating with `Node.spawn_link/2`

* first argument is name of node to connect; second is function to be executed

* `spawn_link/2` runs function on remote node (provided as args), returns PID on linked process

#### Sending messages

* starting a node for `me` with `iex --sname me@localhost`

* sending a function for date from `you` to `me` with following

```
± % iex --sname you@localhost

iex(you@localhost)1> rpid = Node.spawn_link(:me@localhost, fn -> IO.puts(Date.utc_today) end)
#PID<14104.119.0>
2019-11-24
```

#### A note on I/O and Nodes

* though `Node.spawn_link/2` runs function on remote, though local gets the output as local is `group leader` by default

#### Responding to messages

* can use `receive/1` and `send/3` setup to communicate with remote node spawned process

* example usage via `Remote` module in sample code

```
± % iex --sname me@localhost
iex(me@localhost)> c("p24_otp_distribution.exs")
[Remote]

± % iex --sname you@localhost
iex(you@localhost)> c("p24_otp_distribution.exs")
[Remote]


iex(me@localhost)> rpid = Node.spawn_link(:you@localhost, fn -> Remote.chat() end)
#PID<14180.125.0>

iex(me@localhost)> send(rpid, {:hi, self()})
{:hi, #PID<0.111.0>}

iex(me@localhost)> flush()
:sup?
:ok
```

#### A note on communicating between nodes on different networks

* when starting Erlang instance with shared **cookie** as `iex --sname user@localhost --cookie secret-token`; only nodes starting with same cookie can communicate

* `Node.spawn_link/2` is not best choice for distributed node communication; this spawns isolated (unsupervised) processes


### Distributed Tasks

* allows spawning supervised distributed tasks

* will be building a simple supervisor application in an example chat application

* defining supervisor application `mix new p24_chat --sup`

#### Adding Task Supervisor to Supervision Tree

* [lib/p24_chat/application.ex](p24_chat/lib/p24_chat/application.ex)

> * Task supervisor dynamically supervises tasks, started with no children often under a supervision of its own
>
> * Task supervisor added to app's supervision tree with name `P24Chat.TaskSupervisor`

#### Sending messages with Supervised Tasks

* `Task.Supervisor.async/5` starts supervised tasks

> * with `{SupervisorName, remote_node_name}` as first argument
>
> * name of module and function to execute as 2nd and 3rd; arguments to be passed as 4th
>
> * 5th optional argument can describe shutdown options

* [lib/p24_chat.ex](p24_chat/lib/p24_chat.ex) shows definitions for suoervised receive and send

```
± % iex --sname alice@localhost -S mix
Generated p24_chat app
iex(alice@localhost)1> P24Chat.send_message(:bob@localhost, "hey")
:ok
iex(alice@localhost)2> heyyy

nil
iex(alice@localhost)1> P24Chat.send_message(:eve@localhost, "wassup")
stay quiet for a while
:ok


± % iex --sname bob@localhost -S mix
iex(bob@localhost)1> hey

nil
iex(bob@localhost)2> P24Chat.send_message(:alice@localhost, "heyyy")
:ok


± % iex --sname eve@localhost -S mix
iex(eve@localhost)1> hey
```

#### Responding to messages from remote nodes

* [lib/p24_chat.ex](p24_chat/lib/p24_chat.ex) shows definitions `send_message(:eve@localhost, msg)` where using pattern matching calls `busy_people/1` specifically

> * grabs current node's name using `Node.self()` and send it to `busy_people/1`, so message can be sent back


### Testing Distributed Code

* for sample see [test/p24_chat_test.exs](test/p24_chat_test.exs) for test of `send_message`; it will fail for missing node unless managed

> * such test can be tagged and excluded with check for node not running
>
> * app can be configured to not spawn remote tasks in test environment

#### Conditionally excluding tests with tags

* tagging as following shall work, sample can be checked in `test/p24_chat_test.exs` and `test/test_helper.exs`

```
@tag :something
test "does something" do
  assert X == Y
end

exclude = if Node.alive?, do: [], else: [something: true]
ExUnit.start(exclude: exclude)
```

> * `Node.alive?` checks if node is part of distributed system

#### Environment-specific application configuration

* `P24Chat.remote_supervisor/1` manages distributed supervisor, this function can be made configurable based on environment

* via application variables using `config/{dev,test}.exs`

```
## config/dev.exs with distributed supervisor
use Mix.config
config :p24_chat, remote_supervisor: fn(recipient) -> {P24Chat.TaskSupervisor, recipient} end
```

```
## config/test.exs with local supervisor
use Mix.config
config :p24_chat, remote_supervisor: fn(_recipient) -> P24Chat.TaskSupervisor end
```

* update `config/config.exs` file to have

```
import_config "#{Mix.env()}.exs"
```

* update `P24Chat.remote_supervisor/1` function to lookup and use application variable stored function

```
## lib/p24_chat.ex
defp remote_supervisor(recipient) do
  Application.get_env(:chat, :remote_supervisor).(recipient)
end
```

---
