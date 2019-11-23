## concurrency
#

defmodule Xyz do
  def dbl(x) do
    x + x
  end

  def listen do
    receive do
      {:ok, name} -> IO.puts("Hey, #{name}.")
      {:error, reason} -> IO.puts("error: " <> reason)
    end

    listen()
  end

  def crash, do: exit(:bigbang)

  def clean_crash do
    Process.flag(:trap_exit, true)
    spawn_link(Xyz, :crash, [])

    receive do
      {:EXIT, from_pid, reason} ->
        IO.puts("Cleaning after " <> inspect(from_pid) <> ": " <> inspect(reason))
    end
  end

  def cleaner_crash do
    {pid, ref} = spawn_monitor(Xyz, :crash, [])

    receive do
      {:DOWN, ref, :process, from_pid, reason} ->
        IO.puts("Cleaning after " <> inspect(ref) <> " | " <> inspect(from_pid) <> ": " <> inspect(reason))
    end
  end
end

#### * can be onvoked by `Xyz.dbl(10)` in same thread
#
#### * async spawn via `pid = spawn(Xyz, :dbl, [10])`
#
#### * to process receiving, `send(pid, args)` can be used to pass message
#
#### * `spawn_link(Xyz, :crash, [])` to allow originatin/spawned process know if either crashed
#
#### * `spawn_monitor(Xyz, :crash, [])` return Reference along with PID; allows to check without trap on system signals

