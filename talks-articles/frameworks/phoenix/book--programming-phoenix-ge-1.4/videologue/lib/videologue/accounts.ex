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
end
