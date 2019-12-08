defmodule P29Genstage.Producer do
  use GenStage

  @module __MODULE__

  def start_link(initial \\ 0) do
    GenStage.start_link(@module, initial, name: @module)
  end

  def init(counter), do: {:producer, counter}

  def handle_demand(demand, state) do
    events = state..(state + demand - 1) |> Enum.to_list
    {:noreply, events, state + demand}
  end
end
