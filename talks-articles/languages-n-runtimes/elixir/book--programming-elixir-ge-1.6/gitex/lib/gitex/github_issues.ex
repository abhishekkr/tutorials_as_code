defmodule Gitex.GithubIssues do
  require Logger

  @user_agent [{"User-Agent", "Gitex"}]
  @github_api Application.get_env(:gitex, :github_api)

  @moduledoc """
  Handle HTTP fetch for Github Issues.
  """

  def fetch(user, project) do
    Logger.info("fetching github issues for #{user}/#{project}")
    issues_url(user, project)
    |> HTTPoison.get(@user_agent)
    |> handler_response()
  end

  defp issues_url(user, project) do
    "#{@github_api}/repos/#{user}/#{project}/issues"
  end

  defp parse(body), do: Poison.Parser.parse!(body, %{})
  defp handler_response({:ok, %{status_code: 200, body: body}}), do: {:ok, parse(body)}
  defp handler_response({_, %{status_code: code, body: body}}) do
    Logger.error("Github Fetch failed with HTTP #{code}")
    {:error, parse(body)}
  end
end
