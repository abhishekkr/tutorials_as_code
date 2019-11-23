## otp supervisors
defmodule SuperQ do
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

defmodule SuperQ.Application do
  use Supervisor

  def init(state), do: {:ok, state}

  def start(_type, _args) do
    children = [
      SuperQ#, ## state = []
      #Supervisor.child_spec({SuperQ, [10, 12, 20]}, id: SuperQx1) ## state = [10, 12, 20]
    ]
    opts = [strategy: :one_for_one, name: SuperQ.Supervisor]
    {:ok, pid} = Supervisor.start_link(children, opts)
    Supervisor.count_children(pid)
  end
end
