defmodule Duper19.Worker do
  use GenServer, restart: :transient

  @file_chunk 1024*1024

  def start_link(_), do: GenServer.start_link(__MODULE__, :no_args)

  def init(:no_args) do
    Process.send_after(self(), :do_one_file, 0)
    {:ok, nil}
  end

  def handle_info(:do_one_file, _) do
    Duper19.PathFinder.next_path() |> add_result()
  end

  defp add_result(:done) do
    Duper19.Gatherer.done()
    {:stop, :normal, nil}
  end
  defp add_result(path) do
    hash_of(path)
    |> Duper19.Gatherer.result(path)
    send(self(), :do_one_file)
    {:noreply, nil}
  end

  defp hash_of(path) do
    File.stream!(path, [], @file_chunk)
    |> Enum.reduce(
      :crypto.hash_init(:md5),
      fn (block, hash) -> :crypto.hash_update(hash, block) end
      )
    |> :crypto.hash_final()
  end
end
