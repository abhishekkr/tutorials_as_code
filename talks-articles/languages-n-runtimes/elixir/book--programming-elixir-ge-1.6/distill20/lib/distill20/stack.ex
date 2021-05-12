defmodule Distill20.Stack do
  use GenServer
  require Logger

  @vsn "1"

  @mynameis Application.get_env(:distill20, :stack, :stacksvr)

  def purge_ttl(pidme) do
    receive do
    after 1_000 ->
        send(pidme, :purge_ttl)
        purge_ttl(pidme)
    end
  end

  defp do_purge_ttl([], new_state), do: new_state
  defp do_purge_ttl([{elem, nil} | tail], new_state), do: do_purge_ttl(tail, [{elem, nil} | new_state])
  defp do_purge_ttl([{elem, ttl} | tail], new_state) when ttl < 1 do
    Logger.info("purging TTL-d item: #{inspect elem}")
    do_purge_ttl(tail, new_state)
  end
  defp do_purge_ttl([{elem, ttl} | tail], new_state) do
    Logger.info("item: #{inspect elem} has #{inspect ttl} seconds left")
    do_purge_ttl(tail, [{elem, ttl-1} | new_state])
  end

  #### GenServer implementation
  def init(list) do
    spawn(Distill20.Stack, :purge_ttl, [self()])
    {:ok, list}
  end

  def handle_info(:purge_ttl, state) do
    Logger.info("time to purge...")
    {:noreply, do_purge_ttl(state, [])}
  end

  def handle_call(:pop, _from, []), do: {:reply, {nil, nil}, []}
  def handle_call(:pop, _from, [h|tail]) do
    {:reply, h, tail}
  end

  def handle_cast({:push, {nil, _ttl}}, _list), do: raise "That's a nil item to Stack, I'll crash!"
  def handle_cast({:push, {elem, ttl}}, list) when is_integer(ttl) or is_nil(ttl) do
    {:noreply, [{elem, ttl}|list]}
  end
  def handle_cast({:push, {elem, ttl}}, _list), do: raise "#{inspect elem} got an invalid TTL #{inspect ttl}"

  def handle_cast(:cleanup, _list) do
    {:noreply, []}
  end

  def terminate(reason, state) do
    IO.puts("Reason: #{inspect reason}\n\n")
    IO.puts("State: #{inspect state}")
  end


  #### External API
  ## need to be name `start_link` for Supervisor
  def start_link(state) do
    GenServer.start_link(__MODULE__, state, name: @mynameis)
  end

  @doc """
  Pop a Stack.

  # Example

    iex> Distill20.Stack.cleanup()
    iex> Distill20.Stack.pop()
    nil

    iex> Distill20.Stack.push(1)
    iex> Distill20.Stack.push(10)
    iex> Distill20.Stack.push(101)
    iex> Distill20.Stack.pop()
    101
    iex> Distill20.Stack.pop()
    10

    iex> Distill20.Stack.cleanup()
    :ok
  """
  def pop do
    GenServer.call(@mynameis, :pop)
  end

  @doc """
  Push a Stack.

  # Example

    iex> Distill20.Stack.cleanup()
    iex> Distill20.Stack.push(1)
    iex> Distill20.Stack.push(101)
    iex> Distill20.Stack.pop()
    101
    iex> Distill20.Stack.pop()
    1

    iex> Distill20.Stack.push(20)
    iex> Distill20.Stack.push(11, 1)
    iex> :time.sleep(2)
    iex> Distill20.Stack.pop()
    20

    iex> Distill20.Stack.cleanup()
    :ok
  """
  def push(newnum, ttlsec \\ nil) do
    GenServer.cast(@mynameis, {:push, {newnum, ttlsec}})
  end

  @doc """
  Cleanup Stack.

  # Example

    iex> Distill20.Stack.push(1)
    iex> Distill20.Stack.push(10)
    iex> Distill20.Stack.cleanup()
    iex> Distill20.Stack.pop()
    nil
  """
  def cleanup() do
    GenServer.cast(@mynameis, :cleanup)
  end

  ## state migration from 0
  def code_change("0", old_state = current_stack, _extra) do
    new_state = old_state |> Enum.map(&({&1, nil}))
    Logger.info("Changing Stack vsn from 0 to 1")
    Logger.info(inspect old_state)
    Logger.info(inspect new_state)
    {:ok, new_state}
  end
end
