defmodule P29Genstage.Application do
  @moduledoc false

  import Supervisor.Spec, warn: true

  def start(_type, _args) do
    children = [
      worker(P29Genstage.Producer, [0]),
      worker(P29Genstage.ProducerConsumer, []),
      worker(P29Genstage.Consumer, [])
    ]

    opts = [strategy: :one_for_one, name: P29Genstage.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
