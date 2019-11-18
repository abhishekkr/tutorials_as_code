defmodule Speech.Greet do
  @moduledoc """
  Provides greeting function `hey/1` for personalized greeting

  This gets detailed right after 'defmodule ...'
  """

  @doc """
  Returns hey message suffix-ed with param name

  Get detailed right before 'def ...'

  ## Parameters

    - name: String that represents person to be greeted.

  ## Examples

    iex> Speech.Greet.hey("MyName")
    "Hey MyName."
  """
  @spec hey(String.t()) :: String.t()
  def hey(name) do
    "Hey " <> name <> "."
  end
end

# Outputs 'Hello Jane.' to standard output.
Speech.Greet.hey("Jane") |> IO.puts
