defmodule SpawnMsg do
  @moduledoc false

  def greet do
    receive do
      {sender, msg} ->
        send sender, {:ok, "Hey, #{msg}"}
    end
  end
end

defmodule SpawnMultiMsg do
  @moduledoc false

  def greet do
    receive do
      {sender, msg} ->
        send sender, {:ok, "Hey, #{msg}"}
        greet()
    end
  end
end

## client for SpawnMsg
defmodule MultiMsgClient do
  @moduledoc false

  defp do_greet do
    pid = spawn(SpawnMsg, :greet, [])

    send pid, {self(), "You!"}
    receive do
      {:ok, msg} ->
        IO.puts msg
    end
    pid
  end

  def greet_timeout do
    pid = do_greet()
    send pid, {self(), "Again"}
    receive do
      {:ok, msg} ->
        IO.puts msg
      after 500 ->
        IO.puts "Greeter has left."
    end
  end

  def greet do
    pid = do_greet()
    send pid, {self(), "Again"}
    receive do
      {:ok, msg} ->
        IO.puts msg
    end
  end

  def greet_again do
    pid = spawn(SpawnMultiMsg, :greet, [])

    send pid, {self(), "Again"}
    receive do
      {:ok, msg} -> IO.puts msg
    end

    send pid, {self(), "Again"}
    receive do
      {:ok, msg} -> IO.puts msg
    end

    send pid, {self(), "Again"}
    receive do
      {:ok, msg} -> IO.puts msg
    end
  end
end

# running as
# % elixir chapter-15/spawn2.exs
# Hey, You!
MultiMsgClient.greet_timeout()
## MultiMsgClient.greet() ## will get stuck
MultiMsgClient.greet_again()
