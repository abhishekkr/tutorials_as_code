## for script to keep moving to next example even after raise example
## Process.flag(:trap_exit, true)

## Exercise#3
defmodule C15Ex3 do
  def foo(sender) do
    send(sender, {:what, "here we go"})
    send(sender, {:ok, "here we go"})
  end

  def run do
    spawn_link(C15Ex3, :foo, [self()])
    :timer.sleep(500)

    receive do
      {:ok, m} -> IO.puts("someone said: #{m}")
      sth ->
        IO.inspect(sth)
        run()
    after 1000 ->
        IO.puts("nothing happened in time")
    end
  end
end

C15Ex3.run
:timer.sleep(500)


## Exercise#4
defmodule C15Ex4 do
  def fox(sender) do
    send(sender, {:what, "is this"})
    raise "bad fox"
  end

  def run do
    spawn_link(C15Ex4, :fox, [self()])
    :timer.sleep(500)
    receive do
      m -> IO.inspect(m)
    after 1000 ->
        IO.puts("nothing happened in time")
    end
    IO.puts("~") ## shouldn't play unless Process trapped
  end
end

#C15Ex4.run


## Exercise#5
defmodule C15Ex5 do
  def run3 do
    spawn_monitor(C15Ex3, :foo, [self()])
    :timer.sleep(500)

    receive do
      {:ok, m} -> IO.puts("someone said: #{m}")
      sth -> IO.inspect(sth)
    after 1000 ->
        IO.puts("nothing happened in time")
    end
    IO.puts(".")
  end

  def run4 do
    spawn_monitor(C15Ex4, :fox, [self()])
    :timer.sleep(500)
    receive do
      m -> IO.inspect(m)
    after 1000 ->
        IO.puts("nothing happened in time")
    end
    IO.puts("..")
  end
end

C15Ex5.run3
C15Ex5.run4
