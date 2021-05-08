defmodule Stack.Server do
  use GenServer
  alias Stack.Impl

  #### GenServer implementation
  def init(list), do: {:ok, list}

  def handle_call(:pop, _from, list) do
    {val, new_list} = Impl.pop(list)
    {:reply, val, new_list}
  end

  def handle_cast({:push, elem}, list), do: {:noreply, Impl.push(elem, list)}

  def format_status(_reason, [ _pdict, state ]) do
    [data: [{'State', "My current state is '#{inspect state}', :)"}]]
  end

  def terminate(reason, state) do
    IO.puts("current state: #{inspect state}")
    IO.puts("terminating due to #{inspect reason}")
  end
end
