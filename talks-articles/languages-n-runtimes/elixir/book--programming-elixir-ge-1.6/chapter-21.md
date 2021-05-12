
## Chapter.21 Tasks and Agents

> 2 simple to use Elixir abstractions using OTP's features insulating from details
>
> code examples at [dir chapter-21](./chapter-21)


### Tasks

* call to `Task.async` creates separate process, return value is Task Descriptor (PID & ref); used later to identify task later

```
worker = Task.async(fn -> Fib.of(num) end)
## or
worker = Task.async(Fib, :of, [num])
```

* once it needs the value from `async`, `Task.await` passed Task Descriptor waits for background task to finish and return its value

> `await` has a default timeout, that can be increased if needed as `await(task, timeout \\ 5000)`

#### Tasks and Supervision

* tasks are implemented as OTP servers; so can be added to Application's Supervision Tree

* 1st way; can link a task to currently supervised process by calling `start_link` instead of `async`

> while calling `start_link` if running task crashes; when used `async` it crashes only when `await` gets called

* 2nd way; can run directly from a supervisor specifying Task module itself as module to run as

> in application `child_spec` code

```
children = [
  { Task, fn -> do_something_good() end }
]
```

> moving task code into its own module

```
defmodule Appx.TaskA do
  use Task

  def start_link(param) do
    Task.start_link(__MODULE__, :ting_to_run, [param])
  end

  def thing_to_run(param) do
    IO.puts "got #{inspect param} passed to task"
end

### can be supervised as
children = [{Appx.TaskA, 101}]
```

> can't `await` for these as there is no `async`


### Agents

* agent is a background process maintaining state

* can be accessed at different spots within a process/node/multiple-nodes

```
iex(1)> {:ok, pid007} = Agent.start(fn -> 0 end)
{:ok, #PID<0.111.0>}
iex(2)> Agent.get(pid007, &(&1))
0
iex(3)> Agent.update(pid007, &(&1+1))
:ok
iex(4)> Agent.get(pid007, &(&1))
1
```

* can give Agents local/global names to access as `Agent.start(fn -> 0 end, name: MyNum)` that allows `Agent.get(MyNum, &(&1))`

> here `MyNum` is `:Elixir.MyNum`

* sample code at [agent1.exs](./chapter-21/agent1.exs) implements a Word Frequency map usable as

```
iex(1)> WordFrequency.start_link
{:ok, #PID<0.114.0>}
iex(2)> WordFrequency.words
[]
iex(3)> WordFrequency.add_word "alice"
:ok
iex(4)> WordFrequency.add_word "bob"
:ok
iex(5)> WordFrequency.add_word "chad"
:ok
iex(6)> WordFrequency.add_word "bob"
:ok
iex(7)> WordFrequency.add_word "bob"
:ok
iex(8)> WordFrequency.words
["alice", "bob", "chad"]
iex(9)> WordFrequency.count_for "alice"
1
iex(10)> WordFrequency.count_for "bob"
3
iex(11)> WordFrequency.count_for "chad"
1
```


### A Bigger Example

* [anagram.exs](./chapter-21/anagram.exs) as both tasks and agent

> load words in parallel from several separate dictionaries, use agent to store resulting list of words and signatures

* on single machine use

```
% iex chapter-21/anagrams.exs
Erlang/OTP 22 [erts-10.7.2.9]...
Interactive Elixir (1.9.2)...
iex(1)> MyDict.start_link
{:ok, #PID<0.116.0>}
iex(2)> 1..2 |> Enum.map(&("./chapter-21/anagrams.list#{&1}")) |> WordListLoader.load_from_files
[:ok, :ok]
iex(3)> MyDict.anagrams_of "bob"
["bbo", "obb", "bob"]
iex(4)> MyDict.anagrams_of "eve"
["eev", "vee", "eve"]
iex(5)> MyDict.anagrams_of "lice"
nil
iex(6)> MyDict.anagrams_of "alice"
["ecali", "cieal", "ilcea", "laice", "ealic", "ceali", "iceal", "licea", "alice"]
```

* making it distributed; by giving our agent a globally accessible name as `@name {:global, __MODULE__}`; connect different nodes and have `WordListLoader` loading fro both nodes and `MyDict` available at both places


### Agents and Tasks, or GenServer?

* Agents-Tasks for more background tasks; GenServers for servers

> can always wrap Agents-Tasks as in Anagram implementation

---

