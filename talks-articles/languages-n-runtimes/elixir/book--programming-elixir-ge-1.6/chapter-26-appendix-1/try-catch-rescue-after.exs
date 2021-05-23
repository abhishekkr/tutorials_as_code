defmodule The do
  def bar(n) do
    try do
      raise_error(n)
    rescue
      [FunctionClauseError, RuntimeError] ->
        IO.puts("no function match or runtime error")
      error in [ArithmeticError] ->
        IO.inspect(error)
        IO.puts("Uh-oh! Arithmetic error")
        reraise "too late, we're doomed", System.stacktrace
      other_errors ->
        IO.puts("Disaster! #{inspect other_errors}")
    after
      IO.puts("DONE!")
    end
  end

  defp raise_error(0), do: IO.puts("No error.")
  defp raise_error(val=1) do
    IO.puts("No error.")
    1 / (val-1)
  end
  defp raise_error(2) do
    IO.puts("About to call a function that doesn't exist")
    raise_error(99)
  end
  defp raise_error(3) do
    IO.puts("About to try creating a dir with no permission.")
    File.mkdir!("/nodir")
  end
end

try do
  The.bar(1)
after
  try do
    The.bar(2)
  after
    try do
      The.bar(3)
    after
      IO.puts("---")
    end
  end
end
