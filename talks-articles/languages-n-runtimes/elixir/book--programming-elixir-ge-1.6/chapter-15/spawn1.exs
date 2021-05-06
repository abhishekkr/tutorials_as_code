defmodule SpawnMsg do
  @moduledoc """
  Defines a greet to send a message back.
  """

  def greet do
    receive do
      {sender, msg} ->
        send sender, {:ok, "Hey, #{msg}"}
    end
  end
end

## client for SpawnMsg
defmodule Client do
  @moduledoc false

  def greet do
    pid = spawn(SpawnMsg, :greet, [])
    send pid, {self(), "You!"}
    receive do
      {:ok, msg} ->
        IO.puts msg
    end
  end
end

# running as
# % elixir chapter-15/spawn1.exs
# Hey, You!
Client.greet()
