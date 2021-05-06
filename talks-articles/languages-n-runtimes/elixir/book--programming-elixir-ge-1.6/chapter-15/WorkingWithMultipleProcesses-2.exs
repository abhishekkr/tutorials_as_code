defmodule Tom do
  def test do
    jerryx = spawn(Jerry, :mimic, [])
    jerryy = spawn(Jerry, :mimic, [])

    send jerryx, {self(), "fred"}
    send jerryy, {self(), "betty"}

    Tom.talk()
    Tom.talk()
  end

  def talk do
    receive do
      {:ok ,token} -> IO.puts("> #{token} <")
    end
  end
end

defmodule Jerry do
  def mimic do
    receive do
      {sender, token} ->
        send sender, {:ok, token}
        mimic()
    end
  end
end

Enum.each(1..2,
  fn _ ->
    Tom.test()
  end
)
