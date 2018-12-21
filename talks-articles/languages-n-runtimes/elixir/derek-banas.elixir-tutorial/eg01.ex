defmodule Eg01 do
  def main do
    stdio_stuff() # paranthese ain't mandatory but better for specificity
    data_stuff()
    condition_stuff()
  end

  def stdio_stuff do
    name = IO.gets("Your name please? ") |> String.trim ## can pipe return of one function to another
    IO.puts "Hola #{name}"
  end

  def data_stuff do
    an_int = 101 # no max size
    IO.puts "#{an_int} int:#{is_integer(an_int)}"

    a_float = 1.01 # accurate to 16 digits
    IO.puts "#{a_float} float:#{is_float(a_float)}"

    IO.puts ":#{:AnAtom} atom:#{is_atom(:AnAtom)}"
    # non alpha-numberic only Atoms need be surrounded with double-quotes
    IO.puts ":#{:"A To Z"} atom:#{is_atom(:"A To Z")}"
    IO.puts ":#{:"A-Z"} atom:#{is_atom(:"A-Z")}"

    one_to_five = 1..5
    IO.puts "range one to five: #{Enum.count(one_to_five)}"

    string_stuff()
    arith_stuff()
  end

  def string_stuff() do
    a_string = "A String Phrase"
    longer_string = a_string <> " " <> "And More" ## concat
    IO.puts "string length: #{String.length(longer_string)}"
    IO.puts "Egg === egg : #{"Egg" === "egg"}"
    IO.puts "And ? #{String.contains?(longer_string, "And")}"
    IO.puts "First: #{String.first(a_string)}; Index3: #{String.at(a_string, 3)}"
    IO.puts "Substring at8-for7 #{String.slice(longer_string, 8, 7)}"

    IO.inspect "a to z" ## prints internal representation
    IO.inspect String.split("a to z", " ")

    IO.puts "a To z | rev: #{String.reverse("a To z")} | capitalize: #{String.capitalize("a To z")}"
    IO.puts "a To z | upcase: #{String.upcase("a To z")} | downcase: #{String.downcase("a To z")}"

    10 * 1000 |> IO.puts ## piping result for stdout
  end

  def arith_stuff() do
    IO.puts "1+2=#{1+2} | 2-3=#{2-3} | 3*4=#{3*4} | 4/5=#{4/5}"
    IO.puts "5 div 2 = #{div(5,2)} | 5 rem 2 = #{rem(5,4)}"
    ## and a rich math library
  end

  def condition_stuff do
    compare_stuff()
    if_stuff(25)
  end

  def compare_stuff do
    IO.puts "4 == 4.0: #{4 == 4.0} | 4 === 4.0: #{4 === 4.0} | 4 != 4.0: #{4 != 4.0} | 4 !== 4.0: #{4 !== 4.0}" ## == doesn't compare types just value
    IO.puts "1 > 5: #{1 > 5} | 1 < 5: #{1 < 5}"
    IO.puts "1 >= 5: #{1 >= 5} | 5 <= 5: #{1 <= 5}"

    IO.puts "true && false: #{(1==1) and (1==2)} | true || false: #{(1==1) or (1==2)} | not false: #{not (1==2)}"
  end

  def if_stuff(somenumber) do
    if somenumber < 10 do
      IO.puts "single digit: #{somenumber}"
    else
      IO.puts "big enough: #{somenumber}"
    end

    unless somenumber > 99 or somenumber < 10 do
      IO.puts "double digit: #{somenumber}"
    else
      IO.puts "not double digit: #{somenumber}"
    end

    # cond works alike else-if and works on first match, so order is important
    odd_or_even = cond do
      rem(somenumber, 2) == 0 -> :odd
      rem(somenumber, 2) != 0 -> :even
    end

    case odd_or_even do
      :odd -> IO.puts "#{somenumber} is odd"
      :even -> IO.puts "#{somenumber} is even"
      _ -> IO.puts "#{somenumber} is default to weird"
    end

    IO.puts "Ternary: #{if odd_or_even === :odd, do: "oddly", else: "st-even"}"
  end
end
