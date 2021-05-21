defmodule TracerV3 do
  def dump_args(args), do: Enum.map(args, &inspect/1) |> Enum.join(", ")

  defmacro def(definition = {fn_name, _meta, fn_args}, do: content) do
    quote do
      Kernel.def(unquote(definition)) do
        IO.puts "<TRACER>: calling #{unquote(fn_name)} : #{unquote(fn_args) |> TracerV3.dump_args()}"
        result = unquote(content)
        IO.puts "</TRACER>: exiting #{unquote(fn_name)} : #{result}"
        result
      end
    end
  end
end

defmodule TestV3 do
  import Kernel, except: [def: 2]
  import TracerV3, only: [def: 2]

  def puts_sum_three(a,b,c), do: IO.inspect(a+b+c)
  def add_list(list), do: Enum.reduce(list, 0, &(&1+&2))
end

TestV3.puts_sum_three(1,2,3)
TestV3.add_list([1,2,3]) |> IO.inspect()
