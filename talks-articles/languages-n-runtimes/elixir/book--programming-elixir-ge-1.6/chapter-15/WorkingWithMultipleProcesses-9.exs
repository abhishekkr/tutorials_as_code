defmodule WordCount do
  def runner(scheduler) do
    send scheduler, {:free, self()}
    receive do
      {:task, filepath, client} ->
        word = "list"
        send(client, {:answer, filepath, count(word, filepath), self()})
        runner(scheduler)
      {:shutdown} -> exit(:normal)
    end
  end

  def count(word, filepath) do
    File.stream!(filepath)
    |> Stream.flat_map(fn x -> Regex.split(~r/(*UTF)(*UCP)[^\w\/*+-]+/, x) end)
    |> Stream.filter(fn x -> x == word end)
    |> Enum.to_list()
    |> length()
  end
end

#WordCount.count("Chapter", "README.md")
#exit(:shutdown)


defmodule PathWalker do
  def walk(dirpath) do
    runner_count = 10
    filepaths = File.ls!(dirpath)
                |> Enum.filter(&(not File.dir?(&1)))
                |> Enum.map(&("#{dirpath}/#{&1}"))
    {time, result} = :timer.tc(GSched, :run, [filepaths, self(), runner_count, WordCount, :runner])

    IO.puts("\n #.runners |  time (s)")
    IO.puts(" #{runner_count}        |   #{time/1_000_000.0}")

    result
    |> Enum.sort(fn {_, n1}, {_, n2} -> n1 >= n2 end)
    |> print()
  end

  def print(results) do
    IO.inspect(results)
    :ok
  end
end

defmodule GSched do
  def run(items, manager, runner_count, module, foo) do
    1..runner_count
    |> Enum.map(fn(_) -> spawn(module, foo, [self()]) end)
    |> schedule(manager, items, [])
  end

  defp schedule(procs, manager, items, results) do
    receive do
      {:free, pid} when items != [] ->
        [h|tail] = items
        send(pid, {:task, h, self()})
        schedule(procs, manager, tail, results)

      {:free, pid} ->
        send(pid, {:shutdown})
        if length(procs) > 1 do
          schedule(List.delete(procs, pid), manager, items, results)
        else
          results
        end

      {:answer, identity, result, _pid} ->
        schedule(procs, manager, items, [{identity, result} | results])
    end
  end
end

PathWalker.walk(".")
