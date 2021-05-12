defmodule Distill20.Application do
  @moduledoc false

  use Application

  @listen_at System.get_env("DISTILL20_PORT", "8001") |> String.to_integer()

  def start(_type, _args) do
    children = [
      {Distill20.Stack, []},
      Plug.Cowboy.child_spec(
        scheme: :http,
        plug: Distill20,
        options: [port: @listen_at]
      )
    ]

    opts = [strategy: :one_for_one, name: Distill20.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
