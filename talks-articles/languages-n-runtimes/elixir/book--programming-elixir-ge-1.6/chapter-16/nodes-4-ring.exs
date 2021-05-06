##  Nodes 4 is WIP
# Clients to make a Ring, where first sends png to 2nd, 2nd to 3rd...


defmodule TickeRing do
  @name :tickering
  @interval 2_000 # 2sec
  @token_length 5

  def start do
    pid = spawn(__MODULE__, :generator, [[]])
    :global.register_name(@name, pid)
  end

  def register(client_pid) do
    :global.whereis_name(@name)
    |> send({:register, client_pid})
  end

  def generator([]) do
    receive do
      {:register, pid} ->
        prompt_register(pid)
        generator([pid], pid)
    end
  end
  def generator([h|tail], first_client) do
    receive do
      {:register, pid} ->
        prompt_register(pid)
        relink(h, pid, first_client)
        generator([pid,h|tail], first_client)
    after @interval ->
        tick(tail, first_client)
        generator([h|tail], first_client)
    end
  end

  defp relink(old_last, last, first) do
    send(old_last, {:link_to, last})
    send(last, {:link_to, first})
  end

  defp tick([], pid), do: IO.puts("standalone client #{inspect pid}, so no ticking")
  defp tick(_clients, pid) do
    IO.puts("init ping to #{inspect pid}")
    send(pid, {:init_tick, self(), uniq_token()})
  end

  defp prompt_register(pid), do: IO.puts("registering #{inspect pid}")

  defp uniq_token do
    :crypto.strong_rand_bytes(1000)
      |> Base.url_encode64
      |> binary_part(0, @token_length)
  end
end


defmodule Client do
  def start do
    spawn(__MODULE__, :receiver, [:standalone]) |> TickeRing.register()
  end

  def receiver(next_node) do
    receive do
      {:init_tick, _from, token} ->
        IO.puts("init pong##{inspect token} from first client: #{inspect self()}")
        send(next_node, {:tick, self(), token})
        receiver(next_node)
      {:tick, from, token} ->
        IO.puts("pong##{inspect token} for client: #{inspect from}")
        send(next_node, {:tick, self(), token})
        receiver(next_node)
      {:link_to, new_next_node} ->
        IO.puts("#{inspect next_node} is next to #{inspect self()}")
        receiver(new_next_node)
    end
  end
end


## test the ring-ing
defmodule RingTest do
  def run do
    TickeRing.start() |> IO.inspect()

    spawn(Client, :start, [])
    spawn(Client, :start, [])
    spawn(Client, :start, [])
    spawn(Client, :start, [])
    spawn(Client, :start, [])

    receive do
      _ -> raise "shouldn't happen"
    after 2005 -> IO.puts("-------------")
    end
  end
end

RingTest.run
