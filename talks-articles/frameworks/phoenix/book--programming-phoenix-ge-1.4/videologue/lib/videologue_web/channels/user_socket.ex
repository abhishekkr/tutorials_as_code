defmodule VideologueWeb.UserSocket do
  use Phoenix.Socket

  channel "videos:*", VideologueWeb.VideoChannel

  @max_token_age 2 * 7 * 24 * 60 * 60
  @token_salt Application.get_env(:videologue, :phoenix_token_salt)

  @impl true
  def connect(%{"token" => token}, socket, _connect_info) do
    Phoenix.Token.verify(
      socket,
      @token_salt,
      token,
      max_age: @max_token_age
    ) |> do_connect(socket)
  end
  def connect(_params, socket, _connect_info), do: :error

  @impl true
  def id(_socket), do: nil

  defp do_connect({:ok, user_id}, socket) do
    {:ok, assign(socket, :user_id, user_id)}
  end
  defp do_connect({:error, reason}, _socket) do
    :error
  end
end
