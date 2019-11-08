## enum
#### * full list of Enum docs: https://hexdocs.pm/elixir/Enum.html
#### * for lazy enum use Stream docs: https://hexdocs.pm/elixir/Stream.html

Enum.__info__(:functions) |> Enum.each(fn({function, arity}) ->
  IO.puts("#{function}/#{arity}")
end)

#### * all collections except 'tuples' are enumerables
IO.puts("")
colx = ["foo", "bar", "zap", "qp"]
coly = [11, 22, 3, 44, 55]

### all?/2
Enum.all?(colx, &(String.length(&1) == 3)) |> IO.inspect()  ## returns true if all eval to true
Enum.all?(colx, &(String.length(&1) > 1)) |> IO.inspect()


### any?/2
Enum.any?(colx, &(String.length(&1) == 3)) |> IO.inspect()  ## returns true if any eval to true
Enum.any?(colx, &(String.length(&1) > 1)) |> IO.inspect()


### chunk_every/2
Enum.chunk_every(colx, 2) |> IO.inspect()
Enum.chunk_every(colx, 3) |> IO.inspect()


### chunk_by/2
Enum.chunk_by(colx, &(String.length(&1))) |> IO.inspect()
Enum.chunk_by(colx ++ ["kro"], &(String.length(&1))) |> IO.inspect()  ## here 'kro' becomes part of another 3 char list


### map_every/3
Enum.map_every(colx, 3, &(String.length(&1))) |> IO.inspect()
Enum.map_every(coly, 3, &(&1 + 100)) |> IO.inspect()


### each/2
Enum.each(coly, &(IO.puts(&1))) |> IO.inspect()  ## just iterates, doesn't produce a new


### map/2
Enum.map(colx, &(String.length(&1))) |> IO.inspect()
Enum.map(coly, &(&1 + 100)) |> IO.inspect()


### min/1, min/2
Enum.min(coly) |> IO.inspect()
Enum.min(coly, fn -> :missing end) |> IO.inspect()
Enum.min([], fn -> :missing end) |> IO.inspect()


### max/1, max/2
Enum.max(coly) |> IO.inspect()
Enum.max(coly, fn -> :missing end) |> IO.inspect()
Enum.max([], fn -> :missing end) |> IO.inspect()


### filter/2
Enum.filter(colx, fn(x) -> String.length(x) > 2 end) |> IO.inspect()
Enum.filter(coly, fn(x) -> x > 25 end) |> IO.inspect(charlists: false)


### reduce/3
Enum.reduce(coly, 100, fn(x, acc) -> x + acc end) |> IO.inspect(charlists: false)
Enum.reduce(coly, fn(x, acc) -> x + acc end) |> IO.inspect(charlists: false)
Enum.reduce(["x", "y", "z"], "0x", fn(x, acc) -> x <> acc end) |> IO.inspect(charlists: false)


### sort/1, sort/2
Enum.sort(coly) |> IO.inspect()
Enum.sort([name: "A", name: "C", name: "B"]) |> IO.inspect()
Enum.sort([:foo, "bar", 1, true, Kernel]) |> IO.inspect()
Enum.sort(coly, fn(x, y) -> rem(x, 5) > rem(y, 5) end) |> IO.inspect()


### uniq/1
Enum.uniq([1,3,0,1,4,0,9])


### uniq_by/2
Enum.uniq_by([%{a: 1, b: 2}, %{a: 11, b: 2}, %{a: 1, b: 12}], fn(x) -> x.b end) |> IO.inspect()


####
