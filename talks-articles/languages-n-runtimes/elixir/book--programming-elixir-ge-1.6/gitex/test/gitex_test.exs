defmodule GitexTest do
  use ExUnit.Case
  doctest Gitex

  test "greets the world" do
    assert Gitex.hello() == :world
  end
end
