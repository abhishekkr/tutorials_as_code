defmodule Sequence17.Server do
  use GenServer

  def init(num), do: {:ok, num}

  def handle_call(:next_number, _from, current_number) do
    {:reply, current_number, current_number+1}
  end
end
