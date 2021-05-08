defmodule Duper19.WorkerSupervisor do
  @moduledoc false

  use DynamicSupervisor

  @me Application.get_env(:duper19, :worker_supervisor)

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :no_args, name: @me)
  end

  def init(:no_args) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def add_worker do
    {:ok, _pid} = DynamicSupervisor.start_child(@me, Duper19.Worker)
  end
end
