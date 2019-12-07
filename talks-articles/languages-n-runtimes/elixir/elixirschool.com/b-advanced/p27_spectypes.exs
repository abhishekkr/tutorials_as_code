## specification and types

defmodule ExampleSpec do
  @moduledoc false

  @spec plus_10(integer) :: integer
  def plus_10(num) do
    num + 10
  end
end


defmodule TypeX do
  defstruct fname: nil, lname: nil

  @typedoc """
    All t(..) definitions could be used in spec
  """
  @type t(fname, lname) :: %TypeX{fname: fname, lname: lname}

  @typedoc """
    Type representing TypeX struct with only string based names.
  """
  @type t :: %TypeX{fname: String.t(), lname: String.t()}
end

defmodule ExampleTypeX do
  @moduledoc false

  @spec greet(String.t(), TypeX.t()) :: nil
  def greet(msg, person) do
    IO.puts("#{msg} #{person.fname} #{person.lname}!")
  end

  def test do
    greet("Hey", %TypeX{fname: "Jane", lname: "Doe"})
  end
end


ExampleSpec.plus_10(90) |> IO.puts()

ExampleTypeX.test
