defmodule Supseq18.Server do
  use GenServer

  @mynameis Application.get_env(:supseq18, :mynameis)

  #### GenServer implementation
  def init(list), do: {:ok, list}

  def handle_call(:pop, _from, []), do: {:reply, nil, []}
  def handle_call(:pop, _from, [h|tail]), do: {:reply, h, tail}

  def handle_cast({:push, elem}, _list) when is_nil(elem), do: raise "You Have Been Bad!"
  def handle_cast({:push, elem}, list), do: {:noreply, [elem|list]}

  def format_status(_reason, [ _pdict, state ]) do
    [data: [{'State', "My current state is '#{inspect state}', :)"}]]
  end

  def terminate(_reason, current_state) do
    Supseq18.Stash.update(current_state)
  end


  #### External API
  ## need to be name `start_link` for Supervisor
  def start_link([]) do
    GenServer.start_link(__MODULE__, Supseq18.Stash.get(), name: @mynameis)
  end
  def start_link(current_state) do
    GenServer.start_link(__MODULE__, current_state, name: @mynameis)
  end

  def pop do
    GenServer.call(@mynameis, :pop)
  end

  def push(newnum) do
    GenServer.cast(@mynameis, {:push, newnum})
  end
end
