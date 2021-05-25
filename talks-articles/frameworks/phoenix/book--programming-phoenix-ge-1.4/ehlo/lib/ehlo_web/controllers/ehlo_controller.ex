defmodule EhloWeb.EhloController do
  use EhloWeb, :controller

  def user(conn, %{"name" => name}) do
    render(conn, "user.html", name: name)
  end
  def user(conn, _params) do
    render(conn, "user.html")
  end
end
