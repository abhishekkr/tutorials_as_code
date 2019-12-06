## otp distribution

defmodule Remote do
  def chat do
    receive do
      {:hi, node_pid} ->
        send(node_pid, :sup?)
      {:whats_the_date, node_pid} ->
        send(node_pid, Date.utc_today)
    end
  end
end

## you_pid = Node.spawn_link :you@localhost, Remote.chat/0
## send(you_pid, {:hi, self()})
## send(you_pid, {:whats_the_date, self()})
## flush()
