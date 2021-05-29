defmodule VideologueWeb.UserController do
  use VideologueWeb, :controller

  alias Videologue.Accounts
  alias Videologue.Accounts.User

  def index(conn, _params) do
    users = Accounts.list_users()
    render(conn, "index.html", users: users)
  end

  def show(conn, %{"id" => id}) do
    user = Accounts.get_user(id)
    render(conn, "show.html", user: user)
  end

  def new(conn, _params) do
    changeset = Accounts.change_user(%User{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"user" => user_params}) do
    tuple_status_val = Accounts.create_user(user_params)
    do_create(conn,  tuple_status_val)
  end
  def do_create(conn, {:ok, user}) do
    conn
    |> put_flash(:info, "#{user.name} created!")
    |> redirect(to: Routes.user_path(conn, :index))
  end
  def do_create(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> render("new.html", changeset: changeset)
  end
end
