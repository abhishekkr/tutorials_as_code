
## without syntactic sugar of do...end
defmodule Misc, do: (
  def sq(n), do: n * n

  def cube(n), do: (
    n * n * n
  )
)


## pattern matching, also guard clause for +ve int
defmodule Factorial do
  def of(0), do: 1
  def of(n) when is_integer(n) and n > 0, do: n * of(n - 1)
  def of(n), do: raise "unhandled number passed to factorial"

  ## with tail recursion
  def tof(n) when is_integer(n) and n >= 0, do: do_tof(n, 1)
  def tof(n), do: raise "unhandled number passed to factorial"
  defp do_tof(0, f), do: f
  defp do_tof(n, f), do: do_tof(n-1, f * n)
end
IO.puts Factorial.of(0)
IO.puts Factorial.of(1)
IO.puts Factorial.of(5)
## IO.puts Factorial.of(-5)
IO.puts Factorial.tof(0)
IO.puts Factorial.tof(1)
IO.puts Factorial.tof(5)
## IO.puts Factorial.tof(5.5)


## guard clauses
defmodule Guard do
  def is(x) when is_integer(x), do: "integer"
  def is(x) when is_number(x), do: "number"
  def is(x) when is_list(x), do: "list"
  def is(x) when is_atom(x), do: "atom"
  def is(x), do: "some type"
end
IO.puts Guard.is(1)
IO.puts Guard.is(1.0)
IO.puts Guard.is("1.0")
IO.puts Guard.is([1.0])
IO.puts Guard.is(:this)


## default params
defmodule DefaultParam do
  def foo(a, b \\ "B", c \\ "C") do
    IO.puts "3 params: #{a}, #{b}, #{c}"
  end

  def bar(start..stop \\ 1..10) do
    Enum.map start..stop, &(&1*2)
  end
end
DefaultParam.foo("aa")
DefaultParam.foo("aa", "bb")
DefaultParam.foo("aa", "bb", "cc")
IO.inspect DefaultParam.bar()
IO.inspect DefaultParam.bar(5..10)


## pipe operator
defmodule PipeOp do
  def mapreduce(lst, map_fn, reduce_fn) do
    lst
    |> Enum.map(map_fn)
    |> Enum.reduce(reduce_fn)
  end
end
IO.inspect PipeOp.mapreduce([1, 3, 5, 7, 9], &(&1+1), &(&1+&2))

## nested modules
defmodule Out, do: ( defmodule In, do: (def foo, do: "bar") )
defmodule Outer.Inner, do: (def foo, do: "bar")
IO.puts Out.In.foo
IO.puts Outer.Inner.foo

## import directive
defmodule ImportM do
  def foo(lst), do: List.flatten(lst)

  def bar(lst) do
    import List, only: [flatten: 1]
    flatten(lst)
  end
end

## module attributes
defmodule ModuleAttr do
  @attr [1, 2, 3]
  def foo, do: Enum.map(@attr, &(&1+1))

  @attr [:walk, :run]
  def bar, do: Enum.map(@attr, &("> #{&1}"))
end
IO.inspect ModuleAttr.foo
IO.inspect ModuleAttr.bar
