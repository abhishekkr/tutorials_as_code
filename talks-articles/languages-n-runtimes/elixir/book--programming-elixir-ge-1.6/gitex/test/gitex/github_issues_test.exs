defmodule GitexGithubIssuesTest do
  use ExUnit.Case
  doctest Gitex

  import Mock
  import Gitex.GithubIssues, only: [ fetch: 2 ]

  @github_api Application.get_env(:gitex, :github_api)

  describe "#fetch" do
    test "return :ok value for successful request" do
      mock_200 = fn(_url, _headers) ->
        {:ok, %HTTPoison.Response{status_code: 200,
          headers: [{"content-type", "application/json"}],
          body: "{\"created_at\": \"2020-10-16T15:36:41Z\"}"}}
      end
      with_mock HTTPoison, [get: mock_200] do
        {:ok, _} = fetch("elixir-lang", "elixir")
        assert called HTTPoison.get("#{@github_api}/repos/elixir-lang/elixir/issues", :_)
      end
    end

    test "return :error value for successful request" do
      mock_404 = fn(_url, _headers) ->
        {:error, %HTTPoison.Response{status_code: 404,
          headers: [],
          body: "{\"error\": \"Not Found 404\"}"}}
      end
      with_mock HTTPoison, [get: mock_404] do
        {:error, _} = fetch("elixir-lang", "elixirs")
        assert called HTTPoison.get("#{@github_api}/repos/elixir-lang/elixirs/issues", :_)
      end
    end
  end
end
