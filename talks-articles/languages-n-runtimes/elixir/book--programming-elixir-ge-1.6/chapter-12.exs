defmodule UseIfAndUnless do
  def try do
    i1 = if 1 == 2, do: :error, else: :ok
    i2 = if 1 == 1, do: :ok
    u1 = unless 1 == 1, do: :error, else: :ok
    u2 = unless 1 == 2, do: :ok
    if i1 == i2 do
      IO.inspect(i1)
    end
    unless u1 != u2 do
      IO.inspect(u1)
    end
  end
end

UseIfAndUnless.try()


defmodule UseCond do
  defp get_fizzbuzz(num) do
    by3 = rem(num,3) == 0
    by5 = rem(num,5) == 0
    cond do
      by3 and by5 -> "fizzbuzz"
      by5 -> "buzz"
      by3 -> "fizz"
      true -> to_string(num)
    end
  end
  def do_fizzbuzz(0, result), do: result
  def do_fizzbuzz(counter, result), do: do_fizzbuzz(counter-1, [get_fizzbuzz(counter) | result])
  def fizzbuzz(cap), do: do_fizzbuzz(cap, [])

  ## without cond
  def get_nocond_buzz(_n, 0, 0), do: "FizzBuzz"
  def get_nocond_buzz(_n, 0, _), do: "Fizz"
  def get_nocond_buzz(_n, _, 0), do: "Buzz"
  def get_nocond_buzz(n, _, _), do: to_string(n)
  def do_nocond_buzz(0, result), do: result
  def do_nocond_buzz(counter, result), do: do_nocond_buzz(counter-1, [get_nocond_buzz(counter, rem(counter,3), rem(counter,5)) | result])
  def nocond_buzz(cap), do: do_nocond_buzz(cap, [])

  def try do
    fizzbuzz(16) |> IO.inspect()
    nocond_buzz(16) |> IO.inspect()
  end
end

UseCond.try()
  
  
defmodule UseCase do
  defp get_language(rec) do
    case rec do
      ## can't use it like this %{place: _place} = record when place == :FR -> :FR
      %{place: place} = _record when place == :FR -> :FR ## need to be above generic match, else will fail for bad 
      %{place: _place} = record -> record.language
      %{region: _place} = record -> record.lang
      _ -> raise "invalid record"
    end
  end

  def try do
    case File.open("chapter-12.md") do
      {:ok, fyl} ->
        :file.position(fyl, 100)
        IO.puts("first line at 100 position: #{IO.read(fyl, :line)}")
      {:error, reason} -> raise "failed: #{reason}"
    end

    uk1 = %{id: 12, place: :UK, language: :EN, key: :whatev}
    uk2 = %{id: 12, region: :UK, lang: :EN, secret: :whatev}
    get_language(uk1) |> IO.inspect()
    get_language(uk2) |> IO.inspect()
    get_language(%{place: :FR}) |> IO.inspect()
  end
end

UseCase.try()
  
  
defmodule UseExceptions do
  def try do

  end
end

UseExceptions.try()
