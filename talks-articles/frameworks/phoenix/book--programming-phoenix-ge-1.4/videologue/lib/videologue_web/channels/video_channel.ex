defmodule VideologueWeb.VideoChannel do
  use VideologueWeb, :channel

  alias Videologue.{Accounts,Multimedia}
  alias VideologueWeb.AnnotationView

  def join("videos:" <> video_id, params, socket) do
    send(self(), :after_join)
    # :timer.send_interval(5_000, :ping) # is just for simple use-case, can be cleaned up
    since_annotation_id = params["last_seen_id"] || 0
    video_id = String.to_integer(video_id)
    annotations = Multimedia.get_video!(video_id)
                  |> Multimedia.list_annotations(since_annotation_id)
                  |> Phoenix.View.render_many(AnnotationView, "annotation.json")

    ##    {:ok, assign(socket, :video_id, video_id)}   ## 1st change return
    {:ok, %{annotations: annotations}, assign(socket, :video_id, video_id)}
  end

  def handle_info(:ping, socket) do
    count = socket.assigns[:count] || 1
    push(socket, "ping", %{count: count})
    {:noreply, assign(socket, :count, count+1)}
  end

  def handle_info(:after_join, socket) do
    push(socket, "presence_state", VideologueWeb.Presence.list(socket))
    {:ok, _} = VideologueWeb.Presence.track(socket,
      socket.assigns.user_id,
      %{device: "browser"})
    {:noreply, socket}
  end

  def handle_in(event, params, socket) do
    user = Accounts.get_user!(socket.assigns.user_id)
    do_handle_in(event, params, user, socket)
  end

  defp do_handle_in("new_annotation", params, user, socket) do
    Multimedia.annotate_video(user, socket.assigns.video_id, params)
    |> do_new_annoation(user, socket)
  end
  defp do_new_annoation({:ok, annotation}, user, socket) do
    broadcast!(socket, "new_annotation", %{
      id: annotation.id,
      user: VideologueWeb.UserView.render("user.json", %{user: user}),
      body: annotation.body,
      at: annotation.at
    })
    {:reply, :ok, socket}
  end
  defp do_new_annoation({:error, changeset}, _user, socket) do
    {:reply, {:error, %{errors: changeset}}, socket}
  end
end
