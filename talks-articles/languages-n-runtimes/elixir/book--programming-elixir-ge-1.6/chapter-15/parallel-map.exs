defmodule Parallel do
  def pmap(collection, fun) do
    me = self()
    collection
    |> Enum.map(fn (elem) ->
      spawn_link fn -> (send me, { self(), fun.(elem) }) end
    end)
    |> Enum.map(fn (pid) ->
      receive do { ^pid, result } -> result end
    end)
  end

  def mapx(lst, foo) do
    do_mapx(lst, foo, self())
    do_collectx(self(), [], length(lst))
  end

  defp do_mapx([], _foo, _me), do: :ok
  defp do_mapx([h|tail], foo, me) do
    spawn_link fn -> (send me, {me, foo.(h)}) end
    do_mapx(tail, foo, me)
  end

  defp do_collectx(_me, result, 0), do: result
  defp do_collectx(me, result, counter) do
    val = receive do
      {^me, v} -> v
    end
    do_collectx(me, result ++ [val], counter-1)
  end
end


IO.puts(">>")
:timer.tc(Parallel, :mapx, [[1,2,3,4,5], &(&1*2)]) |> IO.inspect()
:timer.tc(Parallel, :pmap, [1..5, &(&1*2)]) |> IO.inspect()
