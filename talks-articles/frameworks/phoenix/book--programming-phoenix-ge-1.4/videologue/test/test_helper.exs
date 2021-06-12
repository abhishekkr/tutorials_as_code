ExUnit.start()
Ecto.Adapters.SQL.Sandbox.mode(Videologue.Repo, :manual)

defmodule Videologue.TestHelpers do
  alias Videologue.{Accounts,Multimedia}

  def user_fixture(attrs \\ %{}) do
    pos = System.unique_integer([:positive])
    {:ok, user} = attrs
                  |> Enum.into(%{
                    name: "Test User #{pos}",
                    username: "user#{pos}",
                    password: attrs[:password] || "this-is-secret"
                  })
                  |> Accounts.register_user()
    user
  end

  def video_fixture(attrs \\ %{}) do
    pos = System.unique_integer([:positive])
    {:ok, video} = attrs
                  |> Enum.into(%{
                    title: "Test Video: #{pos}",
                    url: "http://my.video/#{pos}",
                    description: "this is a test video",
                    category_id: attrs[:category_id] || nil
                  })
                  |> do_video_fixture(attrs[:user_id])
    video
  end
  def do_video_fixture(attrs_map, nil) do
    Multimedia.create_video(attrs_map)
  end
  def do_video_fixture(attrs_map, user_id) do
    Accounts.get_user!(user_id)
    |> Multimedia.create_user_video(attrs_map)
  end

  def category_fixture(name) do
    {:ok, category} = Multimedia.create_category!(name)
    category
  end
end
