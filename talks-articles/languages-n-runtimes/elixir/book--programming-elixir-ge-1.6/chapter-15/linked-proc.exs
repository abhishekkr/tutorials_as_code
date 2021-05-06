## top level knows nothing
defmodule Link1 do
  def sad_foo() do
    :timer.sleep(500)
    exit(:boom)
  end

  def run do
    spawn(Link1, :sad_foo, [])
    receive do
      m -> IO.puts("someone said: #{m}")
    after 1000 ->
        IO.puts("nothing happened in time")
    end
  end
end

Link1.run


## linking 2 processes
defmodule Link2 do
  def run do
    spawn_link(Link1, :sad_foo, [])
    receive do
      m -> IO.puts("someone said: #{m}")
    after 1000 ->
        IO.puts("nothing happened in time")
    end
  end
end

#Link2.run


defmodule Link3 do
  def run do
    Process.flag(:trap_exit, true)
    spawn_link(Link1, :sad_foo, [])
    receive do
      m -> IO.puts("someone said: #{inspect m}")
    after 1000 ->
        IO.puts("nothing happened in time")
    end
  end
end

Link3.run


defmodule Link4 do
  def run do
    spawn_monitor(Link1, :sad_foo, []) |> IO.inspect()
    receive do
      m -> IO.puts("someone said: #{inspect m}")
    after 1000 ->
        IO.puts("nothing happened in time")
    end
  end
end

Link4.run
