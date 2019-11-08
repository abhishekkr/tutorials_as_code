## functions

### anonymous functions
double_it = fn (a) -> 2 * a end
double_it.(10) |> IO.inspect()  ## uses DOT operator between function_name and (<args>)
add_3 = fn (a, b, c) -> a + b + c end
add_3.(11, 22, 33) |> IO.inspect()

#### & shorthand, mandate minimum one argument
add_4 = &(&1 + &2 + &3 + &4)
add_4.(2, 4, 6, 8) |> IO.inspect()


### pattern matching
handle_x = fn
  {:ok, result} -> IO.puts("all is well, got: #{result}")
  {:warning, result} -> IO.puts("something is fishy, got: #{result}")
  {:error} -> IO.puts("boom, panic")
end
handle_x.({:ok, 201})
handle_x.({:warning, 401})
handle_x.({:error})


### named functions
#### * 'def' for public scope, `defp` for private scope
#### * can't invoke def,defp outside defmodule
defmodule FnNamed do
  def ehlo, do: IO.puts("EHLO")
  def hello(message) do
    IO.puts("Hello, #{message}")
  end

  def list_len([]), do: 0
  def list_len([_ | tail]), do: 1 + list_len(tail)
end
FnNamed.ehlo()
FnNamed.hello("how are you")
FnNamed.list_len([])
FnNamed.list_len([1, 3, 5])

#### function naming and arity
defmodule FnNameArity do
  def hello(), do: "hello unknown"
  def hello(name), do: "hello #{name}"
  def hello(title, name), do: "hello #{title} #{name}"
end
FnNameArity.hello() |> IO.inspect()
FnNameArity.hello("John") |> IO.inspect()
FnNameArity.hello("Mr", "John") |> IO.inspect()

#### functions and pattern matching
defmodule FnPatMatch do
  def hello(%{fname: fname}) do ## if needed a specific value from a data structure
    IO.inspect("Hey #{fname}")
  end

  def hey(%{fname: fname, lname: _}) do ## this wouldn't use 'lname' but mandates it's presence in argument
    IO.inspect("Hey #{fname}")
  end

  def hola(%{fname: fname} = person) do ## to get inner value and entire arg as well
    IO.inspect("Hey #{fname}")
    IO.inspect(person)
  end
end
FnPatMatch.hello(%{fname: "John"})
FnPatMatch.hello(%{fname: "John", lname: "Doe"})
FnPatMatch.hey(%{fname: "John", lname: "Doe"})
FnPatMatch.hola(%{fname: "John", lname: "Doe"})

#### private functions
defmodule FnPrivate do
  def greet(name), do: hey() <> name
  defp hey, do: "Hey, "  ## private fn
end
FnPrivate.greet("Jane") |> IO.inspect()

#### guards
defmodule FnGuards do
  def hey(names) when is_list(names) do
    names |> Enum.join(", ") |> hey
  end

  def hey(name) when is_binary(name) do
    greet() <> name
  end

  defp greet, do: "Hey, "
end
FnGuards.hey(["John", "Jane"]) |> IO.inspect()
FnGuards.hey("Jane") |> IO.inspect()

#### default arguments
defmodule FnDefault do
  def hey(name, lang \\ "en"), do: phrase(lang) <> name

  def phrase("en"), do: "Hey, "
  def phrase("es"), do: "Hola, "
  def phrase(lang), do: raise RuntimeError, message: "unhandled lang: #{lang}"
end
FnDefault.hey("Jane")
FnDefault.hey("Jane", "en")
FnDefault.hey("Jane", "es")
##error-case: FnDefault.hey("Jane", "Howdy")

defmodule FnDefaultWithHeader do
  def hey(name, lang \\ "en")  ## definitions with multiple clauses and default values, need header like this

  def hey(names, _) when is_list(names), do: names |> Enum.join(", ") |> hey()
  def hey(name, lang) when is_binary(name), do: phrase(lang) <> name

  def phrase("en"), do: "Hey, "
  def phrase("es"), do: "Hola, "
  def phrase(lang), do: raise RuntimeError, message: "unhandled lang: #{lang}"
end
FnDefaultWithHeader.hey(["John", "Jane"]) |> IO.inspect()
FnDefaultWithHeader.hey("Jane") |> IO.inspect()
