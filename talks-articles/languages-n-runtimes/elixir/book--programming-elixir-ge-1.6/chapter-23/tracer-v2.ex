defmodule TracerV2 do
  defmacro def(definition, do: content) do
    IO.inspect(definition)
    quote do
      Kernel.def(unquote(definition)) do
        unquote(content)
      end
    end
  end
end

defmodule TestV2 do
  import Kernel, except: [def: 2]
  import TracerV2, only: [def: 2]

  def puts_sum_three(a,b,c), do: IO.inspect(a+b+c)
  def add_list(list), do: Enum.reduce(list, 0, &(&1+&2))
end

TestV2.puts_sum_three(1,2,3)
TestV2.add_list([1,2,3]) |> IO.inspect()
