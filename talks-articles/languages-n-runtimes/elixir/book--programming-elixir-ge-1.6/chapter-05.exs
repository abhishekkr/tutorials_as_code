defmodule Fnx do
  ## passing functions as arguments
  def run(foo, param) do
    foo.(param)
  end
end


## simple anon function
list_concat = fn
  a, b -> a ++ b
end
IO.inspect list_concat.([:a, :b], [:c])

## using pattern matching in anon function
tpl_to_lst = fn
  {a, b} -> [a, b]
end
IO.inspect tpl_to_lst.({:this,:that})

## one function multiple bodies
handle_open = fn
  {:ok, file} -> "Read data: #{IO.read(file, :line)}"
  {_, error} -> "Error: #{:file.format_error(error)}"
end
IO.puts Fnx.run(handle_open, File.open("chapter-05.md"))
IO.puts Fnx.run(handle_open, File.open("chapter-x5.md"))

fizz_buzz_base = fn
  0, 0, _ -> "fizzbuzz"
  0, _, _ -> "fizz"
  _, 0, _ -> "buzz"
  _, _, n -> n
end
IO.puts fizz_buzz_base.(0, 0, 1)
IO.puts fizz_buzz_base.(0, 1, 2)
IO.puts fizz_buzz_base.(1, 0, 3)
IO.puts fizz_buzz_base.(1, 2, 4)

fizz_buzz = &(fizz_buzz_base.(rem(&1, 3), rem(&1, 5), &1))
IO.puts fizz_buzz.(9)
IO.puts fizz_buzz.(10)
IO.puts fizz_buzz.(11)
IO.puts fizz_buzz.(15)

## funcion return functions
hello_world = fn ->
  fn ->
    "World!"
  end
end
IO.puts hello_world.().()

## pinned values
defmodule Greet do
  def for(name, salute) do
    fn
      (^name) -> "#{salute} #{name}"
      (_) -> "go away"
    end
  end
end
dduck = Greet.for("Donald", "Hola!")
IO.puts dduck.("Donald")
IO.puts dduck.("Mickey")

## work with tuples
divrem = &{ div(&1, &2), rem(&1, &2) }
IO.inspect divrem.(10, 3)

## work with string
hola = &"hola #{&1}"
IO.puts hola.("Senor")

match_end = &~r/.*#{&1}$/
IO.inspect ("Hola" =~ match_end.("!"))
IO.inspect ("Hola!" =~ match_end.("!"))


len = &Enum.count/1
IO.puts len.([1, 3, 5])
