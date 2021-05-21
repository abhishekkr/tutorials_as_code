defmodule TracerV4 do
  def dump_args(args), do: Enum.map(args, &inspect/1) |> Enum.join(", ")

  defmacro def(definition = {fn_name, _meta, fn_args}, do: content) do
    quote do
      Kernel.def(unquote(definition)) do
        IO.puts "<TRACER>: calling #{unquote(fn_name)} : #{unquote(fn_args) |> TracerV4.dump_args()}"
        result = unquote(content)
        IO.puts "</TRACER>: exiting #{unquote(fn_name)} : #{result}"
        result
      end
    end
  end

  defmacro __using__(_opts) do
    quote do
      import Kernel, except: [def: 2]
      import unquote(__MODULE__), only: [def: 2]
    end
  end
end

defmodule TestV4 do
  use TracerV4

  def puts_sum_three(a,b,c), do: IO.inspect(a+b+c)
  def add_list(list), do: Enum.reduce(list, 0, &(&1+&2))
end

TestV4.puts_sum_three(1,2,3)
TestV4.add_list([1,2,3]) |> IO.inspect()
