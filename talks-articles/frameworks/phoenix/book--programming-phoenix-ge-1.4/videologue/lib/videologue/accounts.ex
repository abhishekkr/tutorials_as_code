defmodule Videologue.Accounts do
  @moduledoc """
  The Accounts Context
  """

  alias Videologue.Accounts.User

  def list_users do
    [
      %User{id: "1", name: "Alice", username: "Alice InChains"},
      %User{id: "2", name: "Bob", username: "Bob Cat"},
      %User{id: "3", name: "Chad", username: "Chad Wick"}
    ]
  end

  def get_user(id), do: list_users() |> Enum.find(fn user -> user.id == id end)

  def get_user_by(params) do
    list_users()
    |> Enum.find(fn user ->
      Enum.all?(params, fn {key, val} -> Map.get(user, key) == val end)
    end)
  end
end
