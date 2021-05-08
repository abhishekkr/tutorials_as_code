defmodule Sequence17.StackServer do
  use GenServer

  @mynameis Application.get_env(:sequence17, :mynameis)

  #### GenServer implementation
  def init(list), do: {:ok, list}

  def handle_call(:pop, _from, []), do: {:reply, nil, []}
  def handle_call(:pop, _from, [h|tail]), do: {:reply, h, tail}

  def handle_cast({:push, elem}, list), do: {:noreply, [elem|list]}

  def format_status(_reason, [ _pdict, state ]) do
    [data: [{'State', "My current state is '#{inspect state}', :)"}]]
  end


  #### External API
  def start(current_state) do
    GenServer.start_link(__MODULE__, current_state, name: @mynameis)
  end

  def pop do
    GenServer.call(@mynameis, :pop)
  end

  def push(newnum) do
    GenServer.cast(@mynameis, {:push, newnum})
  end
end
