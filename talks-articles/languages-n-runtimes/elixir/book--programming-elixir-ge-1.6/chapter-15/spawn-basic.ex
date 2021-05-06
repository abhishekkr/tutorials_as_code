defmodule SpawnBasic do
  @moduledoc """
  Defines a greet to run as a separate process.
  """

  @doc """
  Examples:

  calling a regular function
  iex > SpawnBasic.greet()  ## space disabling doctest
  Hey!
  :ok

  run as a separate process
  iex > spawn(SpawnBasic, :greet, [])
  Hey!
  #PID<0.117.0> ## some PID
  """
  def greet do
    IO.puts "Hey!"
  end
end
