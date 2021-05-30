defmodule VideologueWeb.VideoController do
  use VideologueWeb, :controller

  alias Videologue.Multimedia
  alias Videologue.Multimedia.Video

  def action(conn, _params) do
    args = [conn, conn.params, conn.assigns.current_user]
    apply(__MODULE__, action_name(conn), args)
  end

  def index(conn, _params, current_user) do
    videos = Multimedia.list_user_videos(current_user)
    render(conn, "index.html", videos: videos)
  end

  def new(conn, _params, _current_user) do
    changeset = Multimedia.change_video(%Video{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"video" => video_params}, current_user) do
    case Multimedia.create_user_video(current_user, video_params) do
      {:ok, video} ->
        conn
        |> put_flash(:info, "Video created successfully.")
        |> redirect(to: Routes.video_path(conn, :show, video))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}, current_user) do
    video = Multimedia.get_user_video!(current_user, id)
    render(conn, "show.html", video: video)
  end

  def edit(conn, %{"id" => id}, current_user) do
    video = Multimedia.get_user_video!(current_user, id)
    changeset = Multimedia.change_video(video)
    render(conn, "edit.html", video: video, changeset: changeset)
  end

  def update(conn, %{"id" => id, "video" => video_params}, current_user) do
    video = Multimedia.get_user_video!(current_user, id)

    case Multimedia.update_video(video, video_params) do
      {:ok, video} ->
        conn
        |> put_flash(:info, "Video updated successfully.")
        |> redirect(to: Routes.video_path(conn, :show, video))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", video: video, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}, current_user) do
    video = Multimedia.get_user_video!(current_user, id)
    case Multimedia.delete_video(video) do
      {:ok, _video} ->
        conn
        |> put_flash(:info, "Video deleted successfully.")
        |> redirect(to: Routes.video_path(conn, :index))

      {:error, %Ecto.Changeset{} = changeset} ->
        conn
        |> put_flash(:info, "Video deletion failed.")
        |> redirect(to: Routes.video_path(conn, :index))
    end

  end
end
