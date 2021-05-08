defmodule Duper19.ResultsTest do
  use ExUnit.Case
  alias Duper19.Results

  describe "External API" do
    test "shows no duplicates for unique file set" do
      Results.add_hash_for("0a1p3", "/path/a/p")
      Results.add_hash_for("0a1q3", "/path/a/q")
      Results.add_hash_for("0a1r3", "/path/a/r")
      Results.add_hash_for("0a1s3", "/path/a/s")
      Results.add_hash_for("0a1t3", "/path/a/t")

      assert Results.find_duplicates() == []
      Results.cleanup()
    end

    test "shows duplicates for same file hash" do
      Results.add_hash_for("0a1D3", "/path/a/p")
      Results.add_hash_for("0a1q3", "/path/a/q")
      Results.add_hash_for("0a1r3", "/path/a/r")
      Results.add_hash_for("0a1D3", "/path/a/s")
      Results.add_hash_for("0a1t3", "/path/a/t")

      assert Results.find_duplicates() |> length() == 1
      Results.cleanup()
    end
  end
end
