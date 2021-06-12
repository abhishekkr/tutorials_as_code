defmodule Videologue.Accounts do
  @moduledoc """
  The Accounts Context
  """

  alias Videologue.Repo
  alias Videologue.Accounts.User

  def list_users, do: Repo.all(User)

  def get_user(id), do: Repo.get(User, id)
  def get_user!(id), do: Repo.get!(User, id)

  def get_user_by(params), do: Repo.get_by(User, params)

  def change_user(%User{} = user) do
    User.changeset(user, %{})
  end

  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  def change_registration(%User{} = user, params) do
    User.registration_changeset(user, params)
  end

  def register_user(attrs \\ %{}) do
    %User{}
    |> User.registration_changeset(attrs)
    |> Repo.insert()
  end

  def authenticate_by_username_and_password(username, given_pass) do
    get_user_by(username: username)
    |> do_authenticate_by_username_and_password(given_pass)
  end
  defp do_authenticate_by_username_and_password(nil, _given_pass) do
    Pbkdf2.no_user_verify()
    {:error, :not_found}
  end
  defp do_authenticate_by_username_and_password(user, given_pass) do
    if Pbkdf2.verify_pass(given_pass, user.password_hash) do
      {:ok, user}
    else
      {:error, :unauthorized}
    end
  end
end
