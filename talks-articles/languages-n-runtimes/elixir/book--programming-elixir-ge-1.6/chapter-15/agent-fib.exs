defmodule FibAgent do
  def start_link do
    Agent.start_link(fn -> %{ 0 => 0, 1 => 1 } end)
  end

  def fib(pid, n) when n >= 0 do
    Agent.get_and_update(pid, &do_fib(&1, n))
  end

  defp do_fib(cache, n) do
    case cache[n] do
      nil ->
        {n_1, cache} = do_fib(cache, n-1)
        result = n_1 + cache[n-2]
        {result, Map.put(cache, n, result)}
      hit ->
        {hit, cache}
    end
  end
end


defmodule ABKFibWithoutAgent do
  defp do_fibonacci(-1, _n, _m, result), do: result
  defp do_fibonacci(c, prevprev, prev, result) do
    current = prev + prevprev
    do_fibonacci(c-1, prev, current, [current|result])
  end
  def fibonacci(0), do: []
  def fibonacci(1), do: [0]
  def fibonacci(2), do: [0, 1]
  def fibonacci(n), do: do_fibonacci(n-2, 0, 1, [1,0]) |> hd()
end


IO.puts "FibAgent from Programming Elixir book"

{:ok, agent} = FibAgent.start_link()
:timer.tc(FibAgent, :fib, [agent, 2000]) |> IO.inspect()
:timer.tc(FibAgent, :fib, [agent, 2001]) |> IO.inspect()


IO.puts "My older version of Fib without agent"

:timer.tc(ABKFibWithoutAgent, :fibonacci, [2000]) |> IO.inspect()
:timer.tc(ABKFibWithoutAgent, :fibonacci, [2001]) |> IO.inspect()
