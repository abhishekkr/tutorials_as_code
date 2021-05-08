defmodule Duper19.Gatherer do
  use GenServer

  @me Application.get_env(:duper19, :gatherer)

  ## GenServer impl
  def init(worker_count) do
    Process.send_after(self(), :kickoff, 0)
    {:ok, worker_count}
  end

  def handle_info(:kickoff, worker_count) do
    1..worker_count
    |> Enum.each(fn _ -> Duper19.WorkerSupervisor.add_worker() end)
    {:noreply, worker_count}
  end

  def handle_cast(:done, 1 = _worker_count) do
    report_results()
    System.halt(0)
  end
  def handle_cast(:done, worker_count), do: {:noreply, worker_count - 1}

  def handle_cast({:result, hash, path}, worker_count) do
    Duper19.Results.add_hash_for(hash, path)
    {:noreply, worker_count}
  end

  defp do_report_results([]), do: :done
  defp do_report_results([h|tail]) do
    Enum.join(h, " SAME-AS\n\t") |> IO.puts()
    IO.puts "-------------"
    do_report_results(tail)
  end
  defp report_results do
    IO.puts("Results:\n")
    Duper19.Results.find_duplicates() |> do_report_results()
  end

  ## External API
  def start_link(worker_count) do
    GenServer.start_link(__MODULE__, worker_count, name: @me)
  end

  def done, do: GenServer.cast(@me, :done)

  def result(hash, path), do: GenServer.cast(@me, {:result, hash, path})
end
