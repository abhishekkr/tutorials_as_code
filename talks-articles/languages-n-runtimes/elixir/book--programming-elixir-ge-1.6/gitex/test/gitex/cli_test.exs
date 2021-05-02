defmodule GitexCliTest do
  use ExUnit.Case
  doctest Gitex
  import ExUnit.CaptureIO ## allows capture stdout to test screen printed stuff

  import Gitex.CLI, only: [ parse_args: 1, sort_issues: 1, print_table: 1 ]

  describe "#parse_args" do
    test ":help returned for -h and --help options" do
      assert parse_args(["-h"]) == :help
      assert parse_args(["-h", "anything"]) == :help
      assert parse_args(["--help", "anything"]) == :help
    end

    test "three vales returned if three passed" do
      assert parse_args(["alice", "lixir", "9"]) == [user: "alice", project: "lixir", count: 9]
    end

    test "two vales returned if two passed" do
      assert parse_args(["alice", "lixir"]) == [user: "alice", project: "lixir", count: 4]
    end
  end

  describe "#sort_issues" do
    test "sort list of map by reverse created_at" do
      ordered = ["2021-04-25T20:24:52Z", "2021-04-02T21:12:16Z", "2021-03-27T13:17:56Z", "2021-03-25T08:50:10Z"]

      assert ordered == Enum.shuffle(ordered)
      |> Enum.map(&(%{"created_at" => &1, "other_detials" => String.split(&1, "T")}))
      |> sort_issues()
      |> Enum.map(fn x -> x["created_at"] end)
    end
  end

  describe "#print_table" do
    test "print formatted issues json" do
      sample_issues = [
        %{"number" => 10954, "created_at" => "2021-04-25T20:24:52Z", "title" => "Allow filtering modules from coverage using regex"},
        %{"number" => 10865, "created_at" => "2021-04-02T21:12:16Z", "title" => "Deprecate URI.parse"},
        %{"number" => 10823, "created_at" => "2021-03-27T13:17:56Z", "title" => "Wrong \"incompatible types\" warning with tuples"},
        %{"number" => 10813, "created_at" => "2021-03-25T08:50:10Z", "title" => "Make ranges have a default step of 1"}
      ]

      screen_grab = capture_io fn ->
        print_table(sample_issues)
      end

      assert screen_grab == " #       | created at              | title
 ------- + ----------------------- + --------------------------------------------------
 10954   | 2021-04-25T20:24:52Z    | Allow filtering modules from coverage using regex
 10865   | 2021-04-02T21:12:16Z    | Deprecate URI.parse
 10823   | 2021-03-27T13:17:56Z    | Wrong \"incompatible types\" warning with tuples
 10813   | 2021-03-25T08:50:10Z    | Make ranges have a default step of 1
"
    end
  end
end
