defmodule TracerV4 do
  def dump_args(args), do: Enum.map(args, &inspect/1) |> Enum.join(", ")

  def execute_def(definition = {fn_name, _meta, fn_args}, content, guards \\ nil) do
    quote do
      Kernel.def(unquote(definition)) do
        is_match = if is_nil(unquote(guards)) do
          true
        else
          {result, _binding} = Code.eval_quoted(unquote(guards), binding())
          result
        end
        if is_match do
          IO.ANSI.format([
            :green, "<TRACER>: calling",
            :blue, " #{unquote(fn_name)} : #{unquote(fn_args) |> TracerV4.dump_args()}"
          ]) |> IO.puts
          result = unquote(content)
          IO.ANSI.format([
            :yellow, "</TRACER>: exiting",
            :blue, :bright, " #{unquote(fn_name)} : #{result}"
          ]) |> IO.puts()
          result
        else
          raise "function call for #{unquote(fn_name)} didn't match"
        end
      end
    end
  end

  defmacro def({:when, _meta, [definition, guards]}, do: content) do
    TracerV4.execute_def(definition, content, guards)
  end

  defmacro def(definition, do: content) do
    TracerV4.execute_def(definition, content)
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

  def add1(n) when not is_nil(n), do: n+1
end

TestV4.puts_sum_three(1,2,3)
TestV4.add_list([3,2,3]) |> IO.inspect()
TestV4.add1(10) |> IO.inspect()
TestV4.add1(nil) |> IO.inspect()
