
## Erlang Interoperability

### standard library
#
defmodule ExampleStdlib do
  def timed(func, args) do
    {timed, result} = :timer.tc(func, args)
    IO.puts("Time: #{timed} μs")
    IO.puts("Result: #{result}")
  end
end
ExampleStdlib.timed(&(&1*&1*&1*&1*&1*&1*&1*&1*&1*&1*&1*&1*&1), [100])
#

### string
#
is_list('Example') |> IO.inspect()
is_list("Example") |> IO.inspect()
is_binary("Example") |> IO.inspect()
(<<"Example">> == "Example") |> IO.inspect()

to_charlist("Hey Tonpa") |> :string.words |> IO.inspect()

erlang_string = to_charlist("Hey Tonpa") |> :string.split(" ") |> List.first()
IO.inspect(erlang_string)
is_list(erlang_string) |> IO.inspect()
is_binary(erlang_string) |> IO.inspect()

##################################################
