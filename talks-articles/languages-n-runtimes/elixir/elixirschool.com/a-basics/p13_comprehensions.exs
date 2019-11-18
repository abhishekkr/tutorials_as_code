## comprehensions
### * list comprehensions are syntactic sugar for looping through enumerables

lstx = [11, 22, 33, 44, 55] ## list
lsty = [:what, :why, :where, :which, :how]
lstz = [1, 3, 5]

keylistx = [en: "hello", en: "hi", es: "hola", in: "namaste"] ## keyword list
mapx = %{en: "English", es: "Espanol", in: "India"} ## map
binx = "oye-hoye" ## binary

defmodule X do
  def say(x) do
    IO.inspect("+ #{x}")
  end

  def saykv({k, v}) do
    IO.inspect("+ #{k} => #{v}")
  end
end


### basics
#### * concise statements for 'Enum' and 'Stream' iteration
for num <- lstx, do: num*num |> X.say

lst = for num <- lstx, do: num*num
IO.inspect(lst)

#### * keyword list
for {_k, val} <- keylistx, do: val |> X.say

#### * maps
for {key, val} <- mapx, do: {key, val} |> X.saykv

#### * binaries
for <<c <- binx>>, do: <<c>> |> X.say

#### * pattern matching
for {:en, values} <- keylistx, do: values |> X.say  ## in case of match
for {:de, values} <- keylistx, do: values |> X.say  ## ignored, when no match

#### * using multiple generators, much like nested loops
for num <- lstz, times <- 1..num do
  String.duplicate("~", times) |> X.say
end


### filters
#### * are like guards for comprehensions
import Integer

lst_even = for x <- 1..5, is_even(x), do: x
IO.inspect(lst_even)

lst_div_2_3 = for x <- 1..50, is_even(x), rem(x, 3) == 0, do: x
IO.inspect(lst_div_2_3)


### using :into
#### * `:into` option accepts any structure that implements `Collectable` protocol

map_from_keylist = for {k, v} <- keylistx, into: %{}, do: {k, v}
IO.inspect(map_from_keylist)

bin_from_list = for atom <- lsty, into: "", do: Atom.to_string(atom)
IO.inspect(bin_from_list)

hello = for c <- [72, 101, 108, 108, 111], into: "", do: <<c>>
IO.inspect(hello)
##################################################
