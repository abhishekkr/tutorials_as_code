defmodule P29Genstage.Consumer do
  use GenStage

  @module __MODULE__

  def start_link do
    GenStage.start_link(@module, :state_got_no_impact)
  end

  def init(state) do
    {:consumer, state, subscribe_to: [P29Genstage.ProducerConsumer]}
  end

  def handle_events(events, _from, state) do
    for event <- events do
      IO.inspect({self(), event, state})
    end
    {:noreply, [], state}
  end
end
