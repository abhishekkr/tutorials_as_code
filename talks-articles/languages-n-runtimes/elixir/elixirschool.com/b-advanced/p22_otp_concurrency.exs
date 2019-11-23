## otp concurrency

defmodule SimpleQ do
  use GenServer

  defmacro __using__(_opts) do
    quote do
      @doc """
      Start our queue and link it.
      This is a helper function.
      """
      def start_link(state \\ []) do
        GenServer.start_link(__MODULE__, state, name: __MODULE__)
      end

      @doc """
      GenServer.init/1 callback
      """
      def init(state), do: {:ok, state}
    end
  end

  def init(state), do: {:ok, state}
end


defmodule SyncQ do
  use SimpleQ

  @doc """
  GenServer.handle_call/3 callback
  """
  def handle_call(:dequeue, _from, [value | state]), do: {:reply, value, state}
  def handle_call(:dequeue, _from, []), do: {:reply, nil, []}
  def handle_call({:enqueue, value}, _from, state), do: {:reply, :ok, state ++ [value]}
  def handle_call(:queue, _from, state), do: {:reply, state, state}

  ### client API / helper functions
  def queue, do: GenServer.call(__MODULE__, :queue)
  def enqueue(value), do: GenServer.call(__MODULE__, {:enqueue, value})
  def dequeue, do: GenServer.call(__MODULE__, :dequeue)
end


defmodule AsyncQ do
  use SimpleQ

  @doc """
  GenServer.handle_call/3 callback
  """
  def handle_call(:dequeue, _from, [value | state]), do: {:reply, value, state}
  def handle_call(:dequeue, _from, []), do: {:reply, nil, []}
  def handle_call(:queue, _from, state), do: {:reply, state, state}

  @doc """
  GenServer.handle_cast/2 callback
  """
  def handle_cast({:enqueue, value}, state), do: {:noreply, state ++ [value]}

  ### client API / helper functions
  def queue, do: GenServer.call(__MODULE__, :queue)
  def enqueue(value), do: GenServer.cast(__MODULE__, {:enqueue, value})
  def dequeue, do: GenServer.call(__MODULE__, :dequeue)
end
