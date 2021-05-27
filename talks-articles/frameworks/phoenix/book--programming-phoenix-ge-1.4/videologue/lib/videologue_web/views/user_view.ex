defmodule VideologueWeb.UserView do
  use VideologueWeb, :view

  alias Videologue.Accounts

  def first_name(%Accounts.User{name: name}) do
    name
    |> String.split(" ")
    |> Enum.at(0)
  end
end
