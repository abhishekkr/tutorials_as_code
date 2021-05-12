defmodule Supseq18.Stash do
  use GenServer

  @me Application.get_env(:supseq18, :mystash)


  ## GenServer impl

  def init(init_state), do: {:ok, init_state}

  def handle_call({:get}, _from, state), do: {:reply, state, state}

  def handle_cast({:update, new_state}, _state), do: {:noreply, new_state}


  ## External API

  def start_link(init_state), do: GenServer.start_link(__MODULE__, init_state, name: @me)

  def get, do: GenServer.call(@me, {:get})

  def update(new_state), do: GenServer.cast(@me, {:update, new_state})
end
