defmodule Ticker do
  @interval 2_000 # 2sec
  @name :ticker

  def start do
    pid = spawn(__MODULE__, :generator, [[]])
    :global.register_name(@name, pid)
  end

  def register(client_pid) do
    :global.whereis_name(@name)
    |> send({:register, client_pid})
  end

  def generator(clients) do
    receive do
      {:register, pid} ->
        IO.puts("registering #{inspect pid}")
        generator([pid|clients])
    after
      @interval ->
        IO.puts("ping")
        Enum.each(clients, &(send(&1, {:tick})))
        generator(clients)
    end
  end
end

defmodule Client do
  def start do
    spawn(__MODULE__, :receiver, []) |> Ticker.register()
  end

  def receiver do
    receive do
      {:tick} ->
        IO.puts("pong in client")
        receiver()
    end
  end
end


## start 2 nodes; connect one to another
## run `Ticker.start` on one
## run as many desired `Client.start` on another
