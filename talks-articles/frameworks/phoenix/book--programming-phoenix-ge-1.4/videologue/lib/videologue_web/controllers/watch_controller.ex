defmodule VideologueWeb.WatchController do
  use VideologueWeb, :controller

  alias Videologue.Multimedia

  def show(conn, %{"id" => id}) do
    video = Multimedia.get_video!(id)
    render(conn, "show.html", video: video)
  end
end
