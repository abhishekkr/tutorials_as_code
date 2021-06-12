defmodule VideologueWeb.VideoControllerTest do
  use VideologueWeb.ConnCase, async: true

  alias Videologue.Multimedia

  @create_attrs %{description: "a video about something", title: "first video", url: "http://vid.eo/first"}
  @update_attrs %{description: "video#1 about something", title: "the first video", url: "http://vid.eo/fir/st"}
  @invalid_attrs %{title: nil}

  describe "logged-out user" do
    test "gets redirected for all video actions", %{conn: conn} do
      [
        get(conn, Routes.video_path(conn, :new)),
        get(conn, Routes.video_path(conn, :index)),
        get(conn, Routes.video_path(conn, :show, "101")),
        get(conn, Routes.video_path(conn, :edit, "101")),
        get(conn, Routes.video_path(conn, :update, "101", %{})),
        get(conn, Routes.video_path(conn, :create, %{})),
        get(conn, Routes.video_path(conn, :delete, "101")),
      ] |> Enum.each(fn conn ->
        assert html_response(conn, 302)
        assert conn.halted
      end)
    end
  end

  describe "logged-in user for unauthorized video" do
    setup %{conn: conn, login_as: username}, do: login(conn, username)

    @tag login_as: "alice"
    test "does not access other user's video", %{conn: conn, user: user} do
      user_bob = user_fixture(%{username: "bob"})
      attrs = Map.put(@create_attrs, :user_id, user_bob.id)
      video = video_fixture(%{ attrs | title: "Bob" })

      conn = get(conn, Routes.video_path(conn, :index))
      refute html_response(conn, 200) =~ "Bob"

      {:ok, conn} = recycle(conn) |> conn_assign(user)
      assert_error_sent :not_found, fn ->
        get(conn, Routes.video_path(conn, :show, video))
      end

      assert_error_sent :not_found, fn ->
        get(conn, Routes.video_path(conn, :edit, video))
      end

      assert_error_sent :not_found, fn ->
        put(conn, Routes.video_path(conn, :update, video, video: @create_attrs))
      end

      assert_error_sent :not_found, fn ->
        delete(conn, Routes.video_path(conn, :delete, video))
      end
    end
  end

  describe "logged-in index" do
    setup %{conn: conn, login_as: username}, do: login(conn, username)

    @tag login_as: "alice"
    test "lists all videos", %{conn: conn} do
      conn = get(conn, Routes.video_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Videos"
    end
  end

  describe "logged-in new video" do
    setup %{conn: conn, login_as: username}, do: login(conn, username)

    @tag login_as: "alice"
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.video_path(conn, :new))
      assert html_response(conn, 200) =~ "New Video"
    end
  end

  describe "logged-in create video" do
    setup %{conn: conn, login_as: username}, do: login(conn, username)

    @tag login_as: "alice"
    test "redirects to show when data is valid", %{conn: conn, user: user} do
      conn = post(conn, Routes.video_path(conn, :create), video: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.video_path(conn, :show, id)

      {:ok, conn} = recycle(conn) |> conn_assign(user)
      conn = get(conn, Routes.video_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Video"

      assert Multimedia.get_video!(id).user_id == user.id
    end

    @tag login_as: "alice"
    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.video_path(conn, :create), video: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Video"
    end
  end

  describe "edit video" do
    setup %{conn: conn, login_as: username} do
      {:ok, conn: conn, user: user} = login(conn, username)
      %{video: video} = create_video(user.id)
      {:ok, conn: conn, user: user, video: video}
    end

    @tag login_as: "alice"
    test "renders form for editing chosen video", %{conn: conn, video: video} do
      conn = get(conn, Routes.video_path(conn, :edit, video))
      assert html_response(conn, 200) =~ "Edit Video"
    end
  end

  describe "update video" do
    setup %{conn: conn, login_as: username} do
      {:ok, conn: conn, user: user} = login(conn, username)
      %{video: video} = create_video(user.id)
      {:ok, conn: conn, user: user, video: video}
    end

    @tag login_as: "alice"
    test "redirects when data is valid", %{conn: conn, video: video, user: user} do
      conn = put(conn, Routes.video_path(conn, :update, video), video: @update_attrs)
      assert redirected_to(conn) == Routes.video_path(conn, :show, video)

      {:ok, conn} = recycle(conn) |> conn_assign(user)
      conn = get(conn, Routes.video_path(conn, :show, video))
      assert html_response(conn, 200) =~ "Video updated successfully."
    end

    @tag login_as: "alice"
    test "renders errors when data is invalid", %{conn: conn, video: video} do
      conn = put(conn, Routes.video_path(conn, :update, video), video: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Video"
    end
  end

  describe "delete video" do
    setup %{conn: conn, login_as: username} do
      {:ok, conn: conn, user: user} = login(conn, username)
      %{video: video} = create_video(user.id)
      {:ok, conn: conn, user: user, video: video}
    end

    @tag login_as: "alice"
    test "deletes chosen video", %{conn: conn, video: video, user: user} do
      conn = delete(conn, Routes.video_path(conn, :delete, video))
      assert redirected_to(conn) == Routes.video_path(conn, :index)
      {:ok, conn} = recycle(conn) |> conn_assign(user)
      assert_error_sent 404, fn ->
        get(conn, Routes.video_path(conn, :show, video))
      end
    end
  end

  defp login(conn, username) do
    user = user_fixture(username: username)
    {:ok, conn} = conn_assign(conn, user)
    {:ok, conn: conn, user: user}
  end
  defp conn_assign(conn, user), do: {:ok, assign(conn, :current_user, user)}

  defp create_video(user_id), do: %{video: do_create_video(user_id)}
  defp do_create_video(nil), do: video_fixture(@create_attrs)
  defp do_create_video(user_id) do
    attrs = Map.put(@create_attrs, :user_id, user_id)
    video_fixture(attrs)
  end
end
