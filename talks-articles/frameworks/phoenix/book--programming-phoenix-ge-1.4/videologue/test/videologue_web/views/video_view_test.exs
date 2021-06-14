defmodule VideologueWeb.VideoViewTest do
  use VideologueWeb.ConnCase, async: true
  alias Videologue.Accounts.User
  alias Videologue.Multimedia.Video
  alias Videologue.Multimedia.Category
  import Phoenix.View

  test "renders index.html", %{conn: conn} do
    videos = [%Video{id: "1", title: "dogs"}, %Video{id: "2", title: "cats"}]
    content = render_to_string(VideologueWeb.VideoView,
      "index.html",
      conn: conn,
      videos: videos)

    assert String.contains?(content, "Listing Videos")

    for v <- videos do
      assert String.contains?(content, v.title)
    end
  end

  test "renders new.html", %{conn: conn} do
    owner = %User{}
    changeset = Videologue.Multimedia.change_video(%Video{})
    categories = [%Category{id: 101, name: "cats"}]

    content = render_to_string(VideologueWeb.VideoView,
      "new.html",
      conn: conn,
      changeset: changeset,
      categories: categories)

    assert String.contains?(content, "New Video")
  end
end
