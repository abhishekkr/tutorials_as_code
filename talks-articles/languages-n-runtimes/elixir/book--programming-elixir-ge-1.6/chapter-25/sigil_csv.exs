defmodule SigilCsv do
  @moduledoc "Sigil module for CSV"

  @doc """
  Give ~v sigil parsing CSV to list of lists.

  Example:
  iex> import SigilCsv
  iex> ~v\"""
  ...> 1,2,3
  ...> a,b,c
  ...> \"""
  [[1, 2, 3], ["a", "b", "c"]]
  """
  def sigil_v(records, []) do
    records |> String.split("\n") |> Enum.map(&do_sigil_v/1) |> Enum.reject(&(&1 == [""]))
  end
  def sigil_v([], 'h'), do: []
  def sigil_v(records, 'h') do
    [header|body] = records |> String.split("\n")
    header_list = String.split(header, ",")
                  |> Enum.map(&String.trim_trailing/1)
                  |> Enum.map(&String.to_atom/1)
    body
    |> Enum.map(&do_sigil_v/1)
    |> Enum.reject(&(&1 == [""]))
    |> Enum.map(fn rec -> Enum.zip(header_list, rec) end)
  end

  defp do_sigil_v(record) do
    record |> String.split(",") |> Enum.map(&do_float/1)
  end

  defp do_float(item) do
    val = String.trim_trailing(item) |> Float.parse()
    case val do
      {num, _} -> num
      _ -> item
    end
  end

  defmacro __using__(_opts) do
    quote do
      import unquote(__MODULE__), only: [sigil_v: 2]
    end
  end
end


ExUnit.start

defmodule SigilCsvTest do
  use ExUnit.Case
  use SigilCsv

  describe "sigil csv" do
    test "sigil v without header" do
      assert ~v"""
      1,10,100.1
      alice,bob,eve
      """ == [[1.0, 10.0, 100.1], ["alice", "bob", "eve"]]
    end

    test "sigil v with header" do
      assert ~v"""
      Name,Id,Bill
      Alice,10,100.5
      Bob,11,10
      """h == [
        [Name: "Alice", Id: 10, Bill: 100.5],
        [Name: "Bob", Id: 11, Bill: 10.0]
      ]
    end
  end
end
