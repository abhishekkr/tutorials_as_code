defmodule SigilColor do
  @moduledoc "Sigil module for Color name to RGB/HSB code"

  @color_map [
    rgb: [ red: 0xff0000, green: 0x00ff00, blue: 0x0000ff ],
    hsb: [ red: {0,100,100}, green: {120,100,100}, blue: {240,100,100} ]
  ]

  @doc """
  Give ~c sigil taking color name and returns RGB/HSB code.

  Example:
  iex> import SigilColor
  iex> ~c{red}
  0xff0000
  iex> ~c{red}h
  {0, 100, 100}
  """
  def sigil_c(name, []), do: _c(name, :rgb)
  def sigil_c(name, 'r'), do: _c(name, :rgb)
  def sigil_c(name, 'h'), do: _c(name, :hsb)

  def _c(name, code_type), do: @color_map[code_type][String.to_atom(name)]

  defmacro __using__(_opts) do
    quote do
      import Kernel, except: [sigil_c: 2]
      import unquote(__MODULE__), only: [sigil_c: 2]
    end
  end
end


ExUnit.start

defmodule SigilColorTest do
  use ExUnit.Case
  use SigilColor

  describe "sigil c" do
    test "sigil c red" do
      assert ~c{red} == 0xff0000
    end

    test "sigil c with HSB blue" do
      assert ~c{blue}h == {240, 100, 100}
    end
  end
end
