defmodule P29Genstage.ProducerConsumer do
  use GenStage

  require Integer

  @module __MODULE__

  def start_link do
    GenStage.start_link(@module, :state_got_no_impact, name: @module)
  end

  def init(state) do
    {:producer_consumer, state, subscribe_to: [P29Genstage.Producer]}
  end

  def handle_events(events, _from, state) do
    numbers = events |> Enum.filter(&Integer.is_even/1)
    {:noreply, numbers, state}
  end
end
