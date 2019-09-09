defmodule Parallel do
  def pmap(collection, func) do
    collection
    |> Enum.map(&(Task.async(fn -> func.(&1) end)))
    |> Enum.map(&Task.await/1)
  end
end

defmodule ParallelAdd do
  def main do
    Parallel.pmap 1..1000, &(&1 + 1)
  end
end

n1001 = ParallelAdd.main()
IO.puts n1001
