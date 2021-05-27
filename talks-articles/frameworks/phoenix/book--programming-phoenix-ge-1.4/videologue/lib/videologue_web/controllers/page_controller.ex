defmodule VideologueWeb.PageController do
  use VideologueWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
