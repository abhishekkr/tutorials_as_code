defprotocol Caesar do
  def encrypt(string, shift)
  def rot13(string)
end

defimpl Caesar, for: List do
  @lowercase 97..122
  @uppercase 65..90

  def encrypt(list, shift), do: list |> Enum.map(&do_encrypt(&1, shift))

  def rot13(list), do: encrypt(list, 13)

  defp do_encrypt(char, shift) when char in @lowercase and char+shift < 123 do
    char + shift
  end
  defp do_encrypt(char, shift) when char in @lowercase do
    96 + (char + shift - 122)
  end
  defp do_encrypt(char, shift) when char in @uppercase and char+shift < 91 do
    char + shift
  end
  defp do_encrypt(char, shift) when char in @uppercase do
    64 + (char + shift - 90)
  end
  defp do_encrypt(char, _shift), do: char
end

defimpl Caesar, for: BitString do
  def encrypt(string, shift) do
    String.to_char_list(string)
    |> Caesar.List.encrypt(shift)
    |> List.to_string
  end

  def rot13(string), do:
    encrypt(string, 13)
end


defmodule Exercise3 do
   import(Enum, only: [reduce: 3])

    def each(list, foo) do
      reduce(list, :ok, fn x, acc ->
        foo.(x)
        acc
      end)
    end

    def filter(list, foo) do
      reduce(list, [], fn x, acc ->
        if foo.(x) do
          [x | acc]
        else
          acc
        end
      end) |> do_reverse([])
    end

    def map(list, foo) do
      reduce(list, [], fn x, acc ->
        [foo.(x) | acc]
      end) |> do_reverse([])
    end

  defp do_reverse([], revlst), do: revlst
  defp do_reverse([h|tail], revlst), do: do_reverse(tail, [h| revlst])
end


ExUnit.start

defmodule CaesarTest do
  use ExUnit.Case

  test "list implementation" do
    assert Caesar.rot13('Make It Rain') == 'Znxr Vg Enva'
  end

  test "bitstring implementation" do
    assert Caesar.rot13("Make It Rain") == "Znxr Vg Enva"
  end
end

defmodule Exercise3Test do
  use ExUnit.Case

  test "#each" do
    assert Exercise3.each([1,10], &IO.inspect/1) == :ok
  end

  test "#filter" do
    assert Exercise3.filter([1,10], &(&1<10)) == [1]
  end

  test "#map" do
    assert Exercise3.map([1,10], &(&1+1)) == [2,11]
  end
end
