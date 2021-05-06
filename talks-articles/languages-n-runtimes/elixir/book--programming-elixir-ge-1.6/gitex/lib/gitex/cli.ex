defmodule Gitex.CLI do
  @default_count 4

  @moduledoc """
  Handle cli parsing and dispatch to functions using it.
  """

  def main(argv), do: parse_args(argv) |> process()

  @doc """
  `argv` can be -h or --help which returns :help

  otherwise it's a github er handle,,,project-name and optionally entry count

  returns a tuple of `[user: user, project: project, count: count]`, or `:help`
  """
  def parse_args(argv) do
    #### :parse_the_command_line_1st_iteration
    # parse = OptionParser.parse(argv, switches: [help: :boolean],
    #                                      aliases: [h: :help])
    # case parse do
    #  { [help: true], _, _ } -> :help
    #  { _, [user, project, count], _ } ->
    #    
    #  { _, [user, project], _ } ->
    #    [user: user, project: project, count: @default_count]
    #  _ -> :help
    # end

    #### :refactor_big_function_alert_2nd_iteration
    OptionParser.parse(argv,
      switches: [help: :boolean],
      aliases: [h: :help]
    )
    |> elem(1)
    |> do_parse_args()
  end

  defp do_parse_args([user, project, count]),
    do: [user: user, project: project, count: String.to_integer(count)]

  defp do_parse_args([user, project]), do: [user: user, project: project, count: @default_count]
  defp do_parse_args(_), do: :help

  def process(:help) do
    IO.puts("""
    usage: gitex <user> <project> [<count>]
    """)

    System.halt(0)
  end

  def process(user: user, project: project, count: count) do
    Gitex.GithubIssues.fetch(user, project)
    |> decode_response()
    |> sort_issues()
    |> Enum.take(count)
    |> print_table()
    |> IO.inspect()
  end

  def sort_issues(lst) do
    Enum.sort(lst, fn i1, i2 -> i1["created_at"] >= i2["created_at"] end)
  end

  def filter_open(lst) do
    Enum.filter(lst, fn i1 -> i1["state"] == "open" end)
  end

  defp do_print_table([]), do: :ok

  defp do_print_table([%{"number" => number, "created_at" => created_at, "title" => title} | tail]) do
    IO.puts(
      " #{String.pad_trailing(to_string(number), 7, " ")} | #{
        String.pad_trailing(created_at, 23, " ")
      } | #{title}"
    )

    do_print_table(tail)
  end

  def print_table(lst) do
    IO.puts(" #{String.pad_trailing("#", 7)} | #{String.pad_trailing("created at", 23)} | title")

    IO.puts(
      " #{String.pad_trailing("", 7, "-")} + #{String.pad_trailing("", 23, "-")} + #{
        String.pad_trailing("", 50, "-")
      }"
    )

    do_print_table(lst)
  end

  defp decode_response({:ok, issues}), do: issues

  defp decode_response({:error, error}) do
    IO.puts("Error fetching issues from Github: " <> error["message"])
    System.halt(2)
  end
end
