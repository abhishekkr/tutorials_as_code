## basic data types

### integers
255 |> IEx.Info.info() |> IO.inspect()
0b0101 |> IEx.Info.info() |> IO.inspect()
0o7101 |> IEx.Info.info() |> IO.inspect()
0xF101 |> IEx.Info.info() |> IO.inspect()

### floats, 64bit double precision
3.14 |> IEx.Info.info() |> IO.inspect()
1.0e-10 |> IEx.Info.info() |> IO.inspect()

### booleans, all is truthy except false|nil
true |> IEx.Info.info() |> IO.inspect()
false |> IEx.Info.info() |> IO.inspect()
true |> is_atom() |> IO.inspect()
false |> is_atom() |> IO.inspect()

### atoms
:true |> is_boolean() |> IO.inspect() ## true/false
Kernel |> is_atom() |> IO.inspect() ## module names are atoms when declared
:crypto.strong_rand_bytes(5) |> IO.inspect() ## referencing Erlang modules as Atoms, incl. bult-ins

### strings
"EHLO" |> IEx.Info.info() |> IO.inspect()
"dziękujędziękuję" |> IEx.Info.info() |> IO.inspect()
"EHLO
HOWDY" |> IEx.Info.info() |> IO.inspect()
"EHLO\nHOWDY" |> IEx.Info.info() |> IO.inspect()


## basic operations

### arithmetic operations
IO.inspect(1 + 2 - 3 * 4 / 5)
IO.inspect(div(10, 5) + rem(10, 3))

### boolean operations
IO.inspect(-120 || true)
IO.inspect(210 || false)
IO.inspect(false or 12) ## maps to 'orelse' in Erlang, first arg need to be true or false only
IO.inspect(12 && true)
IO.inspect(12 && nil)
IO.inspect(false and 12) ## maps to 'andalso' in Erlang, first arg need to be true or false only
IO.inspect(!101)
IO.inspect(!nil)
IO.inspect(!true)

### comparison
IO.inspect(1 > 2)
IO.inspect(1 < 2)
IO.inspect(1 >= 2)
IO.inspect(1 <= 2)
IO.inspect(1 != 2)
IO.inspect(1 == 1.0)
IO.inspect(1 === 1.0)
IO.inspect(1 !== 1.0)
#### any 2 Elixir types can be configured with following sort order
####   number < atom < reference < function < port < pid < tuple < map < list < bitstring
IO.inspect(:ehlo > 2)
IO.inspect(%{ehlo: "hi"} > 2)

### string interpolcation
fname = "John"
IO.puts("Ahoy! #{fname}")

### string concatenation
IO.puts("Ahoy! " <> fname)

####
