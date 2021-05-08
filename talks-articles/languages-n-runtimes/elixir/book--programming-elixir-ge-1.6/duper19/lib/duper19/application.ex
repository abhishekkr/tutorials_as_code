defmodule Duper19.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Stash, :noargs},
      {FileSys.DirWalk, :noargs},
      Duper19.Results,
      {Duper19.PathFinder, "."},
      Duper19.WorkerSupervisor,
      {Duper19.Gatherer, 2},
      # Starts a worker by calling: Duper19.Worker.start_link(arg)
      # {Duper19.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_all, name: Duper19.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
