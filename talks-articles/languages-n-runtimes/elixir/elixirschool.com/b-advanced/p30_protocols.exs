## protocols

defmodule SomeType do
  defstruct fname: nil, lname: nil
end

defimpl String.Chars, for: SomeType do
  def to_string(some_type) do
    "#{some_type.fname} #{some_type.lname}"
  end
end

defmodule EgSomeType do
  def test do
    some = %SomeType{fname: "Jane", lname: "Doe"}
    to_string(some) |> IO.puts
  end
end

EgSomeType.test


defprotocol AsAtom do
  def to_atom(data)
end

defimpl AsAtom, for: Atom do
  def to_atom(atom), do: atom
end

defimpl AsAtom, for: BitString do
  defdelegate to_atom(string), to: String
end

defimpl AsAtom, for: List do
  defdelegate to_atom(list), to: List
end

defimpl AsAtom, for: Map do
  def to_atom(map), do: List.first(Map.keys(map))
end

defmodule EgAsAtom do
  import AsAtom

  def test do
    to_atom(:this) |> IO.inspect
    to_atom("that") |> IO.inspect
    to_atom([1, 10]) |> IO.inspect
    to_atom(%{foo: "bar"}) |> IO.inspect
  end
end

EgAsAtom.test
