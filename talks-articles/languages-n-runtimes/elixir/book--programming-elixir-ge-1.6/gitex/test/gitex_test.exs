defmodule GitexTest do
  use ExUnit.Case
  doctest Gitex

  test "#version" do
    assert Gitex.version() == "0.1.0"
  end

  test "#hello" do
    assert Gitex.hello() == :world
  end
end
