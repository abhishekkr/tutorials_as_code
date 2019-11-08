## collections

### lists
lstx = [10, 10.01, :this, "that"]  ## may include multiple types
lsty = [:fast | lstx]  ## prepending is faster than appending
lstz = lstx ++ [:slower]

#### list concatenations
[:fast, :john] ++ [:faster, :doe] |> IO.inspect()

#### list subtraction
["foo", :bar, 10] -- [:foo, :bar, 10.0] |> IO.inspect()

#### head/tail
hd(lsty) |> IO.inspect()
tl(lstz) |> IO.inspect()
[head | tail] = lstx
IO.inspect(head)
IO.inspect(tail)

### tuples
tplx = {10, 10.01, :this}  ## similar to lists but stored contiguously; so modification is expensive, to get size is cheap
#### * useful in cases of returning values paired with pattern matching

### keyword lists
kwlstx = [foo: "bar", dis: "tro"]
kwlsty = [{:foo, "bar"}, {:dis, "tro"}]
kwlstx ++ kwlsty |> IO.inspect()
#### * perf at par with lists
#### * keys are atoms which are ordered and need not be unique
#### * commonly used to pass options


### maps
#### * allow keys of any type and un-ordered
mapx = %{:foo => "bar", "dis" => :tro}
map_with_only_atom_keys = %{foo: "bar", dis: "tro"}  ## only atom key maps can be written as this
IO.inspect(mapx[:foo])
IO.inspect(mapx.foo)  ## dot notation only for atom keys
mapy = %{mapx | foo: "baz"}  ## their own way for patching a map to a new copy; will raise KeyError if ':foo' failed
#### * to avoid KeyError use Map.put/3
Map.put(mapy, :foo, "bas") |> IO.inspect()
Map.put(mapy, :fooz, "baz") |> IO.inspect()

####
