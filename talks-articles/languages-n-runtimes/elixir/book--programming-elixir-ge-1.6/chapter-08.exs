## Keyword Lists
defmodule KWPage do
  @defaults [fg: :black, bg: :white, font: :sans]

  def print_text(text, options \\ []) do
    options = Keyword.merge(@defaults, options)
    IO.puts "Print text: #{inspect(text)}"
    IO.puts "Foreground: #{options[:fg]}"
    IO.puts "Background: #{Keyword.get(options, :bg)}"
    IO.puts "Font: #{Keyword.get(options, :font)}"
    IO.puts "Size: #{Keyword.get(options, :size, 12)}"
    IO.puts "Style: #{inspect Keyword.get_values(options, :style)}"
  end
end
KWPage.print_text("hola", fg: :green, bg: :black, style: :bold, style: :underline)


## Maps
defmodule MapX do
  def partx do
    map = %{fname: "John", lname: "Doe"}
    Map.keys(map) |> IO.inspect
    Map.values(map) |> IO.inspect

    IO.inspect "#{map.fname} #{map[:lname]}"
  end

  def party do
    map = %{fname: "James", lname: "Bond", code: "007"}
    mapy = map |> Map.drop([:fname, :code]) |> Map.put(:fname, "Jane")
    mapy |> Map.has_key?(:fname) |> IO.inspect
    {fname, restMap} = Map.pop(mapy, :fname)
    IO.puts fname
    IO.inspect restMap
  end
end
MapX.partx
MapX.party


## Pattern Matching and Updating Maps
defmodule PatMatAndMap do
  def get_name(map) do
    %{name: name} = map
    name
  end

  def fail_on_missing_name(map) do
    %{name: _} = map
  end

  def for_usage do
    people = [%{name: "Doe"}, %{name: "dummy"}]
    IO.inspect(for person = %{name: name} <- people, name != "dummy", do: person)
  end

  def book(%{name: name, price: price}) when price < 10 do
    IO.puts "#{name} is cheaper than 10"
  end

  def book(%{name: name, price: price}) when price < 100 do
    IO.puts "#{name} is cheaper than 100 but more than 10"
  end

  def book(%{name: name, price: price}) do
    IO.puts "#{name} is costlier than 100, it will be #{price}"
  end

  def need_pin do
    record = %{fname: "John", lname: "Doe", title: "Mr"}
    for key <- [:fname, :lname] do
      %{ ^key => val } = record
      val
    end
  end

  def updateA(_map, k1, v1, k2, v2) do
    %{_map | k1 => v1, k2 => v2}
  end
  def updateB(_map, k, v) do
    Map.put_new(_map, k, v)
  end
end
PatMatAndMap.get_name(%{name: "Jane", foo: 10}) |> IO.inspect()
PatMatAndMap.for_usage() |> IO.inspect()
PatMatAndMap.book(%{name: "Jane", price: 1}) |> IO.inspect()
PatMatAndMap.book(%{name: "Jane", price: 11}) |> IO.inspect()
PatMatAndMap.book(%{name: "Jane", price: 111}) |> IO.inspect()
PatMatAndMap.need_pin() |> IO.inspect()
%{%{a: 1, b: 2} | a: 3} |> IO.inspect()
PatMatAndMap.updateA(%{u: 1, p: 2, d: 3}, :p, 22, :d, 33) |> IO.inspect()
PatMatAndMap.updateB(%{u: 1, p: 2, d: 3}, :a, 4) |> IO.inspect()


## Struct
### code sample at "chapter-08-struct.exs"


defmodule NestedDict do
  def try do
    nd = [
      %{nes: %{ted: %{dic: "t-x"}, ma: "p"}},
      %{nes: %{ted: %{dic: "y-y"}, ma: "a"}},
      %{nes: %{ted: %{dic: "h-z"}, ma: "t"}}
    ]

    get_in(hd(nd), [:nes]) |> IO.inspect()
    get_in(hd(nd), [:nes, :ma]) |> IO.inspect()
    pop_in(hd(nd), [:nes, :ted]) |> IO.inspect()
    put_in(hd(nd), [:nes, :ma], "per") |> IO.inspect()
    update_in(hd(nd), [:nes, :ma], &(&1 <> "pper")) |> IO.inspect()
    get_and_update_in(hd(nd), [:nes, :ma], &{&1, &1 <> "p-er"}) |> IO.inspect()

    dic_with_z = fn (:get, collection, next_fn) ->
      for row <- collection do
        if String.contains?(row.nes.ted.dic, "z") do
          next_fn.(row)
        end
      end
    end
    get_in(nd, [dic_with_z, :nes, :ted]) |> IO.inspect()
  end
end
NestedDict.try()


defmodule UseAccess do
  def try do
    lst = [
      %{name: "Batman", travel: {"car", "bike", "ropes"}},
      %{name: "Superman", travel: {"fly", "run"}},
      %{name: "Luther", travel: {"shoulders", "gadgets"}}
    ]
    mp = %{ax: %{ay: {1, 2}, az: "12"}, bx: %{by: {3, 4}, bz: "34"}, cx: %{cy: {5, 6}, cz: "56"}}

    get_in(lst, [Access.all(), :name]) |> IO.inspect()
    get_in(lst, [Access.at(1), :name]) |> IO.inspect()

    get_in(lst, [Access.all(), :travel, Access.elem(1)]) |> IO.inspect()
    get_and_update_in(lst, [Access.all(), :travel, Access.elem(1)], fn (val) -> {val, String.reverse(val)} end) |> IO.inspect()

    get_in(lst, [Access.all(), Access.key(:travel)]) |> IO.inspect()
    get_in(mp, [Access.key!(:bx), :by, Access.elem(0)]) |> IO.inspect()
    get_and_update_in(mp, [Access.key(:bx), :bz], &({&1, "PQR"})) |> IO.inspect()

    Access.pop(mp, :cx) |> IO.inspect()
  end
end
UseAccess.try()


defmodule UseSets do
  def try do
    ax = 4..10 |> Enum.into(MapSet.new)
    bx = 7..11 |> Enum.into(MapSet.new)

    MapSet.member?(ax, 5) |> IO.inspect()
    MapSet.union(ax, bx) |> IO.inspect()
    MapSet.union(bx, ax) |> IO.inspect()
    MapSet.difference(ax, bx) |> IO.inspect()
    MapSet.difference(bx, ax) |> IO.inspect()
    MapSet.intersection(ax, bx) |> IO.inspect()
    MapSet.intersection(bx, ax) |> IO.inspect()
  end
end
UseSets.try()
