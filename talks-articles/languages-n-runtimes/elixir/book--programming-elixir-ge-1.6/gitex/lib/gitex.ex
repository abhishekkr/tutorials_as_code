defmodule Gitex do
  @moduledoc """
  Documentation for Gitex.
  """

  @doc """
  Version of Gitex

  ## Examples

      iex> Gitex.version()
      "0.1.0"

  """
  def version do
    Mix.Project.config()[:version]
  end

  @doc """
  This could be any text describing the function.
  Nothing to do with the doctest.

  ## Example

      iex> name = "JaneDoe"
      iex> Gitex.hello(name)
      "Hi! JaneDoe"

      iex> Gitex.hello()
      :world

  """
  def hello(name), do: "Hi! " <> name
  def hello, do: :world
end
