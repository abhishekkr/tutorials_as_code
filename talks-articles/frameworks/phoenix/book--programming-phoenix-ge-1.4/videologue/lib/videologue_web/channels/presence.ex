defmodule VideologueWeb.Presence do
  @moduledoc """
  Provides presence tracking to channels and processes.

  See the [`Phoenix.Presence`](http://hexdocs.pm/phoenix/Phoenix.Presence.html)
  docs for more details.
  """
  use Phoenix.Presence, otp_app: :videologue,
                        pubsub_server: Videologue.PubSub

  def fetch("videos:" <> _video_id, entries) do # to match only videos subtopics
    users = entries
            |> Map.keys()
            |> Videologue.Accounts.list_users_by_ids()
            |> Enum.into(%{}, fn user ->
              {to_string(user.id), %{username: user.username}}
            end)

    for {key, %{metas: metas}} <- entries, into: %{} do
      {key, %{metas: metas, user: users[key]}}
    end
  end
end
