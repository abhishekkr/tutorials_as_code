defmodule Duper19.Results do
  @moduledoc false

  use GenServer

  @me Application.get_env(:duper19, :results)

  ## GenServer impl

  def init(:no_args), do: {:ok, %{}}

  def handle_cast({:add, hash, path}, results) do
    results = Map.update(
      results, # map to update
      hash,    # key
      [path],  # if new key
      fn existing -> [path | existing] end # op if key exists
    )
    {:noreply, results}
  end

  def handle_cast(:cleanup, _results), do: {:noreply, %{}}

  def handle_call(:find_duplicates, _from, results) do
    {:reply, reused_hash(results), results}
  end

  def reused_hash(results) do
    results
    |> Enum.filter(fn {_hash, paths} -> length(paths) > 1 end)
    |> Enum.map(fn {_hash, paths} -> paths end )
  end

  ## External API
  def start_link(_init_state) do
    GenServer.start_link(__MODULE__, :no_args, name: @me)
  end

  def add_hash_for(hash, path) do
    GenServer.cast(@me, {:add, hash, path})
  end

  def find_duplicates do
    GenServer.call(@me, :find_duplicates)
  end

  def cleanup do
    GenServer.cast(@me, :cleanup)
  end
end
