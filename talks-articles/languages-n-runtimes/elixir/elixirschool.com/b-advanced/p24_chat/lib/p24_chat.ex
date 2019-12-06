defmodule P24Chat do
  @moduledoc false

  @module __MODULE__

  @doc """
  receive_message just displays received message onto standard output
  """
  def receive_message(msg) do
    IO.puts msg
  end

  def busy_user(from) do
    send_message(from, "stay quiet for a while")
  end

  def send_message(:eve@localhost, _) do
    spawn_task(@module, :busy_user, :eve@localhost, [Node.self()])
  end
  def send_message(recipient, msg) do
    spawn_task(@module, :receive_message, recipient, [msg])
  end

  defp spawn_task(module, func, recipient, args) do
    recipient
    |> remote_supervisor()
    |> Task.Supervisor.async(module, func, args)
    |> Task.await()
  end

  defp remote_supervisor(recipient) do
    {P24Chat.TaskSupervisor, recipient}
  end
end
