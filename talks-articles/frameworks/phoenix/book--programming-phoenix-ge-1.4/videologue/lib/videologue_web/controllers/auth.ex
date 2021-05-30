defmodule VideologueWeb.Auth do
  import Plug.Conn
  alias Videologue.Accounts
  alias VideologueWeb.Router.Helpers, as: Routes

  def init(opts), do: opts

  def call(conn, _opts) do
    user_id = get_session(conn, :user_id)
    user = user_id && Accounts.get_user(user_id)
    assign(conn, :current_user, user)
  end

  def login(conn, user) do
    conn
    |> assign(:current_user, user)
    |> put_session(:user_id, user.id)
    |> configure_session(renew: true)
  end

  def logout(conn), do: configure_session(conn, drop: true)

  def authenticate_user(conn, _opts) when is_nil(conn.assigns.current_user) do
    conn
    |> Phoenix.Controller.put_flash(:error, "You are not logged in.")
    |> Phoenix.Controller.redirect(to: Routes.page_path(conn, :index))
    |> halt()
  end
  def authenticate_user(conn, _opts) do
    conn
  end
end
