defmodule VideologueWeb.AuthTest do
  use VideologueWeb.ConnCase, async: true
  alias VideologueWeb.Auth
  alias Videologue.Accounts.User

  setup %{conn: conn} do
    conn = conn
           |> bypass_through(VideologueWeb.Router, :browser)
           |> get("/")
    {:ok, %{conn: conn}}
  end


  describe "#authenticate_user" do
    test "halts when no current_user", %{conn: conn} do
      conn = Auth.authenticate_user(conn, [])
      assert conn.halted
    end

    test "does not halt for existing current_user", %{conn: conn} do
      conn = conn
             |> assign(:current_user, %User{})
             |> Auth.authenticate_user([])
      refute conn.halted
    end
  end

  describe "#login" do
    test "puts user in session", %{conn: conn} do
      login_conn = conn
                   |> Auth.login(%User{id: 101})
                   |> send_resp(:ok, "")
      next_conn = get(login_conn, "/")
      assert get_session(next_conn, :user_id) == 101
    end
  end

  describe "#logout" do
    test "removes user from session", %{conn: conn} do
      logout_conn = conn
                   |> put_session(:user_id, 101)
                   |> Auth.logout()
                   |> send_resp(:ok, "")
      next_conn = get(logout_conn, "/")
      refute get_session(next_conn, :user_id)
    end
  end

  describe "#call" do
    test "puts session user in assigns", %{conn: conn} do
      user = user_fixture(%{username: "alice"})
      conn = conn
             |> put_session(:user_id, user.id)
             |> Auth.call(Auth.init([]))
      assert conn.assigns.current_user.id == user.id
    end

    test "with no seesion user sets assign current_user to nil", %{conn: conn} do
      conn = Auth.call(conn, Auth.init([]))
      refute conn.assigns.current_user
    end
  end
end
