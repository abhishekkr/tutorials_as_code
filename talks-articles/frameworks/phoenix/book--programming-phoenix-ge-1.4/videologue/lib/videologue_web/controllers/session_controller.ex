defmodule VideologueWeb.SessionController do
  use VideologueWeb, :controller

  alias Videologue.Accounts
  alias VideologueWeb.Auth

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, %{"session" => %{"username" => username, "password" => password}}) do
    tuple_status_val = Accounts.authenticate_by_username_and_password(username, password)
    do_create(conn, tuple_status_val)
  end
  defp do_create(conn, {:ok, user}) do
    conn
    |> Auth.login(user)
    |> put_flash(:info, "Welcome back #{user.name}!")
    |> redirect(to: Routes.page_path(conn, :index))
  end
  defp do_create(conn, {:error, _reason}) do
    conn
    |> put_flash(:error, "Invalid credentials, try SignUp if you are new here.")
    |> redirect("new.html")
  end

  def delete(conn, _params) do
    conn
    |> Auth.logout()
    |> put_flash(:info, "You have been Logged out!")
    |> redirect(to: Routes.page_path(conn, :index))
  end
end
