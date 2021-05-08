defmodule Stash do
  use GenServer

  @me Application.get_env(:duper19, :mystash)


  ## GenServer impl

  def init(init_state), do: {:ok, init_state}

  def handle_call({:get, bucket}, _from, state) do
    data = Map.get(state, bucket)
    {:reply, data, state}
  end

  def handle_cast({:update, bucket, data}, state) do
    new_state = put_in(state, [bucket], data)
    {:noreply, new_state}
  end


  ## External API

  def start_link(_init_state), do: GenServer.start_link(__MODULE__, %{}, name: @me)

  def get(bucket), do: GenServer.call(@me, {:get, bucket})

  def update(bucket, new_state), do: GenServer.cast(@me, {:update, bucket, new_state})
end
