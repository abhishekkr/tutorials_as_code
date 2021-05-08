defmodule Duper19.PathFinder do
  @moduledoc false

  use GenServer

  @me Application.get_env(:duper19, :pathfinder)

  ## GenServer impl

  def init(path), do: {:ok, FileSys.DirWalk.ls(path)}

  def handle_call(:next_path, _from, filepath) do
    {:reply, filepath, FileSys.DirWalk.next}
  end

  def handle_cast(:cleanup, _state), do: {:noreply, FileSys.DirWalk.reset()}

  ## External API
  def start_link(rootpath) do
    GenServer.start_link(__MODULE__, rootpath, name: @me)
  end

  def next_path do
    GenServer.call(@me, :next_path)
  end

  def cleanup do
    GenServer.cast(@me, :cleanup)
  end
end
