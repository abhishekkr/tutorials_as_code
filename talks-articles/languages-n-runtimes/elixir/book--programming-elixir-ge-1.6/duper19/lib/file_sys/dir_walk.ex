defmodule FileSys.DirWalk do
  @moduledoc false

  use GenServer
  require Logger

  @me Application.get_env(:duper19, :dirwalker)

  defstruct filelist: [], dirlist: []

  defp files_in({:ok, filelist}, dirpath), do: filelist |> Enum.map(&("#{dirpath}/#{&1}"))
  defp files_in({:error, _}, dirpath) do
    Logger.error("ignore dir: #{dirpath}")
    []
  end

  defp walk(%FileSys.DirWalk{filelist: [], dirlist: []}), do: {:done, %FileSys.DirWalk{}}
  defp walk(%FileSys.DirWalk{filelist: [], dirlist: [path|tail]}) do
    alllist = File.ls(path) |> files_in(path)
    dirlist = alllist |> Enum.filter(&(File.dir?(&1)))
    filelist = alllist |> Enum.filter(&(File.regular?(&1)))
    walk(%FileSys.DirWalk{filelist: filelist, dirlist: dirlist ++ tail})
  end
  defp walk(%FileSys.DirWalk{filelist: [new_file|tail], dirlist: q}) do
    {new_file, %FileSys.DirWalk{filelist: tail, dirlist: q}}
  end

  ## GenServer impl
  def init(:no_args) do
    stash = Stash.get(@me)
    state = case stash do
      nil -> %FileSys.DirWalk{}
      _ -> stash
    end
    {:ok, state}
  end

  def handle_call({:ls, dirpath}, _from, state) do
    state = Map.update(state, :dirlist, [dirpath], fn exis -> [dirpath|exis] end)
    {new_file, new_state} = walk(state)
    {:reply, new_file, new_state}
  end

  def handle_call(:next, _from, state) do
    {new_file, new_state} = walk(state)
    {:reply, new_file, new_state}
  end

  def handle_call(:reset, _from, _state) do
    {:reply, :done, %FileSys.DirWalk{}}
  end

  def terminate(_reason, current_state) do
    Stash.update(@me, current_state)
  end

  ## External API
  def start_link(_init_state) do
    GenServer.start_link(__MODULE__, :no_args, name: @me)
  end

  def ls(dirpath), do: GenServer.call(@me, {:ls, dirpath})

  def next, do: GenServer.call(@me, :next)

  def reset, do: GenServer.call(@me, :reset)
end
