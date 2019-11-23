
## OTP Supervisors

> sample code at [p23_otp_supervisors.exs](p23_otp_supervisors.exs)
>
> specialized process monitoring other process to create fault-tolerance

### Configuration

* `Supervisor.start_link/2` defines child processes, supervision strategy and other options

* to create a new project with supervision tree `mix new my_queue --sup`; code for MyQueue would go in `lib/my_queue.ex` and supervisor code will go in `lib/my_queue/application.ex`

```my_queue.ex
defmodule MyQueue do
  use GenServer

  def start_link(state \\ []), do: GenServer.start_link(__MODULE__, state, name: __MODULE__)

  def init(state), do: {:ok, state}

  def handle_call(:dequeue, _from, [value | state]), do: {:reply, value, state}
  def handle_call(:dequeue, _from, []), do: {:reply, nil, []}
  def handle_call(:queue, _from, state), do: {:reply, state, state}

  def handle_cast({:enqueue, value}, state), do: {:noreply, state ++ [value]}

  def queue, do: GenServer.call(__MODULE__, :queue)
  def enqueue(value), do: GenServer.cast(__MODULE__, {:enqueue, value})
  def dequeue, do: GenServer.call(__MODULE__, :dequeue)
end
```

```application.ex
defmodule MyQueue.Application do
  use Supervisor

  def start(_type, _args) do
    children = [ ## id has to be unique, typically managed by child_spec/1
      MyQueue ## state = []
      ## Supervisor.child_spec({MyQueue, [10, 12, 20]}, id: SuperQx1) ## state = [10, 12, 20]
    ]
    opts = [strategy: :one_for_one, name: MyQueue.Supervisor]
    {:ok, pid} = Supervisor.start_link(children, opts)
    Supervisor.count_children(pid)
  end
end
```

* when run `iex -S mix`, MyQueue is automatically started


* different supervision strategies are

> * `:one_for_one` only restart failed process
>
> * `:one_for_all` restarts all child processes on failure
>
> * `:rest_for_one` restar failed process and any process started after it


### Child Specification

* module given as a child to Supervisor must implement `child_spec/1` to define how to start/stop/restart children

* `use GenServer`, `use Supervisor` and `use Agent` macros automatically define it for us

* when needed to define ourselves, something similar to below could be used

```
def child_spec(opts) do
  %{
    id: MyQueue,                              ## required key, used to identify
    start: {__MODULE__, :start_link, [opts]}, ## required key, module/fn/args to call
    shutdown: 5_000,     ## optional key, defines child's behavior on shutdown
    restart: :permanent, ## optional key, approach on crash
    type: :worker        ## optional key, can be :worker/:supervisor, defaults :worker
  }
end
```


* options for optional key `shutdown` are as follows

> * `:brutal_kill` immediately kill child
>
> * any positive integer (time in milliseconds) for supervisor to wait before getting killed, `5000` is default for `:worker` type
>
> * `:infinity` where Supervisor waits indefinitely before killing child process, default for `:supervisor` type


* values for optional key `restart` mean following

> * `:permanent` makes child always restarted, default
>
> * `:temporary` never restarts child process
>
> * `:transient` means only restart if child terminates abnormally


### DynamicSupervisor

* when children list isn't known on app start and need to be started on demand, DynamicSupervisor is used

* this only supprts `:one_for_one` supervision strategy

```
options = [
  name: MyDynQueue.Supervisor,
  strategy: :one_for_one
]

DynamicSupervisor.start_link(options)
```

* then to start a new MyDynQueue dynamically, we'll use `start_child/2` that gets supervisor and child spec

```
{:ok. pid} = DynamicSupervisor.start_child(MyDynQueue.Supervisor, MyDynQueue)
```


### Task Supervisor

* tasks have their own specialized `Task.Supervisor`, internally using `DynamicSupervisor`

#### Setup

```
children = [
  {Task.Supervisor, name: ExampleApp.TaskSupervisor, restart: :transient}
]

{:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)
```

* major difference here is default strategy being `:temporary` (never restarted)


#### Supervised Tasks

* with supervisor started, `start_child/2` used to create Supervised Task

```
{:ok, pid} = Task.Supervisor.start_child(ExampleApp.TaskSupervisor, fn -> some_work end)
```

---
