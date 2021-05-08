defmodule Stack do
  @moduledoc false

  @server Stack.Server
  @mynameis Application.get_env(:sequence17, :mynameis)

  def start(current_state) do
    GenServer.start_link(@server, current_state, name: @mynameis)
  end

  def pop do
    GenServer.call(@mynameis, :pop)
  end

  def push(newnum) do
    GenServer.cast(@mynameis, {:push, newnum})
  end
end
