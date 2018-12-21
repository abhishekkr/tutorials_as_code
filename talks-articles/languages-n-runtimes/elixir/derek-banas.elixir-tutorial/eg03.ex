defmodule Eg03 do
  def main do
    recursion_stuff()
    enumerable_stuff()
    list_comprehension_stuff()
    error_stuff(0)
    spawn_stuff()
    csp_stuff()
  end

  def recursion_stuff() do
    IO.puts "factorial of 5: #{rec_factorial(5)}"
    IO.puts "sum of list elements: #{rec_sum([0,1,2,3,4,5])}"

    rec_loop(5, 3, &(IO.puts ">> #{&1} <<"))
  end

  def rec_factorial(n) do
    if n <= 1 do
      1
    else
      n * rec_factorial(n-1)
    end
  end

  def rec_sum([]), do: 0
  def rec_sum([fst|rst]) do
    fst + rec_sum(rst)
  end

  def rec_loop(max, min, foo) do ## not needed as 'Enum.?' works
    if max >= min do
      foo.(max)
      rec_loop(max-1, min, foo)
    end
  end

  def enumerable_stuff() do
    IO.puts "Even list: #{Enum.all?([1,4], fn(n) -> rem(n,2) == 0 end)}"
    Enum.each([1,8], fn(n) -> IO.puts ">> #{n*3} <<" end)
    lst3 = Enum.map([1,8], fn(n) -> n*3 end)
    st3 = Enum.reduce(lst3, fn(n, sum) -> n + sum end)
    IO.puts "sum of 3*lst3 #{st3}"

    IO.inspect Enum.uniq([1,2,2,4,5,5,7])
  end

  def list_comprehension_stuff() do
    dbl_lst = for n <-[1,11], do: n*2
    IO.inspect dbl_lst
    even_lst = for n <- [1,2,4], rem(n,2) == 0, do: n
    IO.inspect even_lst
  end

  def error_stuff(n) do
    err = try do
      10/n
    rescue
      ArithmeticError -> "Stupid rules"
    end
    IO.puts err
  end

  def spawn_stuff() do
    pidx = spawn(fn() -> rec_loop(5, 2, &(IO.puts ">> #{&1} <<")) end)
    spawn(fn() -> rec_loop(9, 6, &(IO.puts ";> #{&1} <;")) end)
    IO.inspect pidx
  end

  def csp_stuff() do
    msg_self() 
    msg_spawns()
  end

  def msg_self() do
    send(self(), {:john, 132})
    receive do
      {:jane, uid} -> IO.puts "Spy GX #{uid}"
      {:john, uid} -> IO.puts "Spy Doe #{uid}"
    end

    send(self(), {:jane, 123}) ## either this or above could land either receive
    receive do
      {:jamie, uid} -> IO.puts "Spy JayMe #{uid}"
    after
      100 -> IO.puts "Whoops, time out for message." ## time in millisec
    end
  end

  def msg_spawns() do
    pidx = spawn(
      fn() ->
        receive do
          {:jane, uid} -> IO.puts "Spy GI #{uid}"
        after
          250 -> IO.puts "Whoops, time out for message."
        end
      end
    )

    spawn(
      fn() ->
        send(pidx, {:jane, 145})
      end 
    )
  end
end
