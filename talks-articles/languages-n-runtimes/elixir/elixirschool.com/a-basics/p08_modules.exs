## modules
#### * to create named functions, to group functions in a namespace, define a struct to organize code efficiently

defmodule ExampleModule do
  def otp_release do
    IO.puts(System.otp_release)
  end
end
ExampleModule.otp_release

defmodule ExampleModule.Ex do  ## way to have nested namespace
  def version do
    IO.puts(System.version)
  end
end
ExampleModule.Ex.version

### module attributes
#### * Elixir modules have few reserved attributes, like 'moduledoc, doc, behavior'
defmodule ExampleModule.A do  ## way to have nested namespace
  @greeting "Hey"

  def greet(name) do
    ~s(#{@greeting} #{name}) |> IO.puts
  end
end
ExampleModule.A.greet("Jane")


### structs
defmodule ExStruct.User do
  defstruct name: "Jane", roles: []
end
defmodule ExStructDerive.User do    ## example allow/ban fields to be inspected
  @derive {Inspect, only: [:user]}  ## also allows 'except'

  defstruct user: %ExStruct.User{}, tags: []
end

defmodule ExStruct.ExUser do
  def users do
    janeX = %ExStruct.User{}
    IO.inspect(janeX)
    johnX = %ExStruct.User{name: "John"}
    IO.inspect(johnX)
    adminBobX = %ExStruct.User{name: "Bob", roles: [:admin]}
    IO.inspect(adminBobX)

    bobX = %{adminBobX | roles: []}  ## updated like maps
    IO.inspect(bobX)

    newBobX = %ExStructDerive.User{tags: ["normal"]}
    IO.inspect(newBobX)
  end
end
ExStruct.ExUser.users()


### composition

#### alias
defmodule AliasExampleX do
  alias ExampleModule.Ex

  def x, do: Ex.version
end
AliasExampleX.x()

defmodule AliasExampleY do
  alias ExampleModule.Ex, as: Xex ## to avoid alias conflicts

  def x, do: Xex.version
end
AliasExampleY.x()

defmodule AliasExampleZ do
  alias ExampleModule.{Ex, A} ## to alias multiple modules

  def x, do: Ex.version |> A.greet()
end
AliasExampleZ.x()

#### import
defmodule ImportExampleX do
  import List, only: [last: 1]

  def x(lst), do: last(lst) |> IO.inspect()
end
ImportExampleX.x([22, 33, 11])

#### import : filtering
#### * to import specific or avoid, can filter using ':only' and ':except' options
#### * can also import only functions or only macros using ':functions' or ':macros'
defmodule ImportExampleY do
  import List, only: :functions

  def x(lst), do: last(lst) |> IO.inspect()
end
ImportExampleY.x([22, 33, 11])

#### require
#### * allows using macros, not functions from specified module
defmodule SomeMacro do
  defmacro if_not(condition, do: foo) do
    quote do
      if(!unquote(condition), do: unquote(foo))
    end
  end
end

defmodule RequireExampleX do
  require SomeMacro

  def x do
    SomeMacro.if_not false, do: IO.puts("macro call using require")
  end
end
RequireExampleX.x
##### * 'defmacrop' for private macros

#### use
#### * 'use' macro enable another module to modify current module, it invokes '__using__/1' macro
defmodule UseX do
  defmacro __using__(_opts) do
    quote do
      def hey(name), do: "Hey #{name}."
    end
  end
end
defmodule ExampleUseX do
  use UseX
end
ExampleUseX.hey("Jane") |> IO.inspect()

defmodule UseY do
  defmacro __using__(opts) do
    greeting = Keyword.get(opts, :greeting, "Hi")

    quote do
      def hey(name), do: unquote(greeting) <> " " <> name <> "."
    end
  end
end
defmodule ExampleUseY do
  use UseY, greeting: "Hi"
end
ExampleUseY.hey("Jane") |> IO.inspect()

#### * one popular case is 'use ExUnit.Case, async: true'
