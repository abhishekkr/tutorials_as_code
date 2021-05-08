defmodule Supseq18.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Supseq18.Stash, []},
      {Supseq18.Server, []}
      # {Supseq18.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :rest_for_one, name: Supseq18.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
