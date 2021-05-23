defmodule SigilLine do
  @moduledoc "Sigil module for Lines to List"

  @doc """
  Give ~l sigil taking multiline and returns each line as a list item.

  Example:
  iex> import SigilLine
  iex> ~l \"""
  ...> alice
  ...> bob
  ...> eve
  ...> \"""
  ["alice", "bob", "eve"]
  """
  def sigil_l(lines, opts) do
    lines |> String.trim_trailing() |> String.split("\n")
  end

  def sigil_L(lines, opts) do
    sigil_l(lines, opts)
  end
end


ExUnit.start

defmodule SigilLineTest do
  use ExUnit.Case
  import SigilLine

  test "line sigil l" do
    i = "you"
    assert ~l"""
    alice
    bob
    eve
    #{i}
    """ == ["alice", "bob", "eve", "you"]
  end

  test "line sigil L" do
    assert ~L"""
    alice
    bob
    eve
    #{i}
    """ == ["alice", "bob", "eve", "\#{i}"]
  end
end
