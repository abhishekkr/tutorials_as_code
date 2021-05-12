defmodule Fib do
  def of(0), do: 0
  def of(1), do: 1
  def of(n) do
    :timer.sleep(1)
    Fib.of(n-1) + Fib.of(n-2)
  end
end

defmodule FibTask do
  def run(num) do
    IO.puts "Start the task"
    worker = Task.async(fn -> Fib.of(num) end)
    ## can be also written as
    #
    ## worker = Task.async(Fib, :of, [num])
    #
    IO.puts "Move on"
    IO.puts "Wait for the task"
    result = Task.await(worker, 140000)
    IO.puts "Result: #{inspect result}"
  end
end

FibTask.run(0)
FibTask.run(7)
FibTask.run(20)
