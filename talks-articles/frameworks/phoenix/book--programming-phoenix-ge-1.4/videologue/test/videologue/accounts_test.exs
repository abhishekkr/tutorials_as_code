defmodule Videologue.AccountsTest do
  use Videologue.DataCase, async: true

  alias Videologue.Accounts
  alias Videologue.Accounts.User

  describe "register_user/1" do
    @valid_attrs %{username: "testuser", name: "test user", password: "cryptography"}
    @invalid_attrs %{}

    test "with valid data inserts" do
      assert {:ok, %User{id: id} = user} = Accounts.register_user(@valid_attrs)
      assert user.name == "test user"
      assert user.username == "testuser"
      assert [%User{id: ^id}] = Accounts.list_users()
    end

    test "with invalid data no inserts" do
      assert {:error, _changeset} = Accounts.register_user(@invalid_attrs)
      assert [] == Accounts.list_users()
    end

    test "enforce unique usernames" do
      assert {:ok, %User{id: id}} = Accounts.register_user(@valid_attrs)
      assert {:error, changeset} = Accounts.register_user(@valid_attrs)
      assert %{username: ["has already been taken"]} == errors_on(changeset)
      assert [%User{id: ^id}] = Accounts.list_users()
    end

    test "doesn't accept long usernames" do
      attrs = Map.put(@valid_attrs, :username, String.duplicate("?", 65))
      assert {:error, changeset} = Accounts.register_user(attrs)
      assert %{username: ["should be at most 64 character(s)"]} == errors_on(changeset)
      assert [] = Accounts.list_users()
    end

    test "doesn't accept password less than 8 chars" do
      attrs = Map.put(@valid_attrs, :password, "1234567")
      assert {:error, changeset} = Accounts.register_user(attrs)
      assert %{password: ["should be at least 8 character(s)"]} == errors_on(changeset)
      assert [] = Accounts.list_users()
    end
  end

  describe "authenticate_by_username_and_password/2" do
    @pass "i need a secret"

    setup do
      {:ok, user: user_fixture(password: @pass)}
    end

    test "returns user for correct password", %{user: user} do
      assert {:ok, auth_user} =
        Accounts.authenticate_by_username_and_password(user.username, @pass)
      assert auth_user.id == user.id
    end

    test "returns unauthorized error for bad password", %{user: user} do
      assert {:error, :unauthorized} =
        Accounts.authenticate_by_username_and_password(user.username, "badpass")
    end

    test "returns not found error for bad user" do
      assert {:error, :not_found} =
        Accounts.authenticate_by_username_and_password("baduser", @pass)
    end
  end
end
