defmodule FibSvr do
  def fib(scheduler) do
    send scheduler, {:free, self()}

    receive do
      {:fib, n, client} ->
        send(client, {:answer, n, fibonacci(n), self()})
        fib(scheduler)
      {:shutdown} -> exit(:normal)
    end
  end

  defp do_fibonacci(-1, _n, _m, result), do: result
  defp do_fibonacci(c, prevprev, prev, result) do
    current = prev + prevprev
    do_fibonacci(c-1, prev, current, [current|result])
  end
  def fibonacci(0), do: []
  def fibonacci(1), do: [0]
  def fibonacci(2), do: [0, 1]
  def fibonacci(n), do: do_fibonacci(n-2, 0, 1, [1,0]) |> Enum.reverse()
end


defmodule Sched do
  def run(num_procs, module, foo, to_calculate) do
    (1..num_procs)
    |> Enum.map(fn(_) -> spawn(module, foo, [self()]) end)
    |> schedule(to_calculate, [])
  end

  defp schedule(procs, q, results) do
    receive do
      {:free, pid} when q != [] ->
        [h|tail] = q
        send(pid, {:fib, h, self()})
        schedule(procs, tail, results)

      {:free, pid} ->
        send(pid, {:shutdown})
        if length(procs) > 1 do
          schedule(List.delete(procs, pid), q, results)
        else
          Enum.sort(results, fn {n1,_}, {n2,_} -> n1 <= n2 end)
        end

      {:answer, number, result, _pid} ->
        schedule(procs, q, [{number, result} | results])
    end
  end

  def drive(list_to_calculate) do
    Enum.each(1..10, fn num_procs ->
        {time, result} = :timer.tc(Sched, :run, [num_procs, FibSvr, :fib, list_to_calculate])

        if num_procs == 1 do
          IO.puts(inspect result)
          IO.puts("\n #  time (s)")
        end
        :io.format "~2B   ~.2f~n", [num_procs, time/1_000_000.0]
      end 
    )
  end
end

### driving the Sched
List.duplicate(5000, 20) |> Sched.drive()




##### other implementations below and Benchmarking

defmodule MyFib do
  def fibonacci(number) do
   Enum.reverse(fibonacci_do(number))
  end

  def fibonacci_do(0), do: [0]
  def fibonacci_do(1), do: [1|fibonacci_do(0)]
  def fibonacci_do(number) when number > 1 do
    [x, y|_] = all = fibonacci_do(number-1)
    [x + y|all]
  end
end

defmodule RosettaCodeFib do
  def fibonacci(number) do
    Enum.map(0..number, fn(n) -> fib(n) end)
  end

  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: fib(0, 1, n-2)

  def fib(_, prv, -1), do: prv
  def fib(prvprv, prv, n) do
    next = prv + prvprv
    fib(prv, next, n-1)
  end
end

defmodule ThomasFib do
  def fibonacci(number) do
    Enum.map(0..number, fn(n) -> fib(n) end)
  end

  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: fib(n-1) + fib(n-2)
end

defmodule Benchmark do
  @impls [ThomasFib, RosettaCodeFib, MyFib, FibSvr]
  #@impls [MyFib, FibSvr, MyFib, FibSvr, MyFib, FibSvr]
  @three [0, 1, 1, 2]
  @five @three ++ [3, 5]
  @ten @five ++ [8, 13, 21, 34, 55]
  @twenty @ten ++ [89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]
  @thirty @twenty ++ [10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811,
                      514229, 832040]
  @forty_five @thirty ++ [1346269, 2178309, 3524578, 5702887, 9227465, 14930352,
                          24157817, 39088169, 63245986, 102334155, 165580141,
                          267914296, 433494437, 701408733, 1134903170]

  @numbers %{
    3 => @three,
    5 => @five,
    10 => @ten,
    20 => @twenty,
    30 => @thirty,
    45 => @forty_five
  }

  def measure(module, input, expected_output) do
    {time, ^expected_output} = :timer.tc(module, :fibonacci, [input])
    time
  end

  def run do
    Enum.each(@impls, fn(module) ->
      IO.puts("Benchmarking #{module}")
      run_impl(module)
    end)
  end

  def run_impl(module) do
    Enum.each(@numbers, fn({num, expected}) ->
      time = measure(module, num, expected)
      IO.puts("Computed fib num #{num} in #{time} time")
    end)
  end
end

#Benchmark.run
