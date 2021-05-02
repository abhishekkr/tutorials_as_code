
defmodule UseStr do
  def heredoc do
    IO.write "
    this is
    a new
    line
    "

    ## heredoc notation fixes multiline string leading space issue
    IO.write """
    this is
    a new
    line
    """
  end

  def sigil do
    ~C[a\n113\n#{110+5}] |> IO.inspect() ## char list w/ no escaping or interpolation
    ~c"a\n113#{110+5}" |> IO.inspect() ## char list, escaped & interpolated just like single-quoted string
    ~S[a\n113\n#{110+5}] |> IO.inspect() ## string with no escaping/interpolation
    ~s/a\n113\n#{110+5}/ |> IO.inspect() ## string with escaping/interpolation

    ~N{2021-04-29 22:47:51} |> IO.inspect() ## Naive Raw DateTime
    ~D<2021-04-29> |> IO.inspect() ## Date
    ~T[22:47:59] |> IO.inspect() ## Time
    ~T[22:47:59.2321] |> IO.inspect() ## Time
    ~R[a.*1+4] |> IO.inspect() ## regex with no escaping/interpolation
    ~r"a.*#{1+4}" |> IO.inspect() ## regex with escaping/interpolation

    ~W[big bad bi#{'l'}l] |> IO.inspect() ## list w/o escaping/interpolation
    ~w[big bad bi#{'l'}l] |> IO.inspect() ## list w/ escaping/interpolation
    ~w[big bad bi#{'l'}l]s |> IO.inspect() ## returns as strings, default
    ~w[big bad bi#{'l'}l]a |> IO.inspect() ## returns as atoms
    ~w[big bad bi#{'l'}l]c |> IO.inspect() ## returns as char-lists

    ~w"""
    ack
    bat
    cat
    """ |> IO.inspect()
    ~r"""
    hello
    """i |> IO.inspect()
  end

  def squote do
    s = 'single' |> Enum.reverse()
    is_list(s) |> IO.inspect()
    length(s) |> IO.inspect()
    [66,65,84] |> IO.inspect()
    :io.format("~w~n", [s]) ## ~w forces s to be written as erlang terms
    List.to_tuple(s) |> IO.inspect()
    [66,65,84] ++ [84,69,82] |> IO.inspect()
    s ++ [0] |> IO.inspect() ## append NULL to make IEx return char codes
    'bat' ++ 'ter' |> IO.inspect()
    'bat' -- 'ter' |> IO.inspect()
    List.zip(['abc', '123']) |> IO.inspect()
    [h|_tail] = s
    IO.puts(h)

    ['bat' | 'ball'] |> IO.inspect() ## here 'ball' gets appended as list to element 'bat'
    [?h, ?e, ?l, ?l, ?o] == 'hello'
  end

  def parse_digits([?- | nlst]), do: do_parse_digits(nlst, 0) * -1
  def parse_digits([?+ | nlst]), do: do_parse_digits(nlst, 0)
  def parse_digits(nlst), do: do_parse_digits(nlst, 0)
  defp do_parse_digits([], val), do: val
  defp do_parse_digits([h|tail], val) when h in '0123456789', do: do_parse_digits(tail, (val*10) + (h - ?0))
  defp do_parse_digits([h|_], _val), do: raise "Invalid digit: #{[h]}"
end

UseStr.heredoc()
UseStr.sigil()
UseStr.squote()
UseStr.parse_digits('-123') |> IO.inspect()
UseStr.parse_digits('+423') |> IO.inspect()
UseStr.parse_digits('23') |> IO.inspect()
## UseStr.parse_digits('2b3') ## raises error


## EXERCISE
defmodule StringNBinaries1to3 do
  def is_printable([]), do: true
  def is_printable([h|_tail]) when h < 32 or h > ?~ do ## 32 for space
    false
  end
  def is_printable([_h|tail]), do: is_printable(tail)

  def anagram?(w1, w2), do: w1 == Enum.reverse(w2)
end

StringNBinaries1to3.is_printable(' 12ab~') |> IO.inspect()
StringNBinaries1to3.is_printable('\n12ab~') |> IO.inspect()

StringNBinaries1to3.anagram?('love', 'evol') |> IO.inspect()
StringNBinaries1to3.anagram?('love', 'evil') |> IO.inspect()


## StringNBinaries #4
defmodule StringNBinaries4 do
  defp do_split_op([], result, _op), do: result
  defp do_split_op([h|tail], result, op) when h == 32, do: do_split_op(tail, result, op)
  defp do_split_op([h|tail], result, op) when h == op, do: [op, result, do_split_op(tail, [], op)]
  defp do_split_op([h|tail], result, op), do: do_split_op(tail, result ++ [h], op)
  
  defp to_num(x) when is_integer(x), do: x
  defp to_num(x) when is_float(x), do: x
  defp to_num(x), do: List.to_integer(x)
  defp do_op([?/|tail], ?/), do: Enum.map(tail, &(to_num(&1))) |> Enum.reduce(&(&2 / &1))
  defp do_op([?*|tail], ?*), do: Enum.map(tail, &(to_num(&1))) |> Enum.reduce(&(&2 * &1))
  defp do_op([?+|tail], ?+), do: Enum.map(tail, &(to_num(&1))) |> Enum.reduce(&(&2 + &1))
  defp do_op([?-|tail], ?-), do: Enum.map(tail, &(to_num(&1))) |> Enum.reduce(&(&2 - &1))
  defp do_op(lst, _op), do: lst
  defp do_divide(formula) do
    [h|tail] = do_split_op(formula, [], ?/)

    cond do
      h == ?/ -> do_op([h|tail], ?/)
      true -> formula
    end
  end
  defp do_multiply(formula) do
    [h|tail] = do_split_op(formula, [], ?*)

    cond do
      h == ?* -> [h | Enum.map(tail, &(do_divide(&1)))]
      true -> do_divide(formula)
    end
    |> do_op(?*)
  end
  defp do_addition(formula) do
    [h|tail] = do_split_op(formula, [], ?+)

    cond do
      h == ?+ -> [h | Enum.map(tail, &(do_multiply(&1)))]
      true -> do_multiply(formula)
    end
    |> do_op(?+)
  end
  defp do_subtract(formula) do
    [h|tail] = do_split_op(formula, [], ?-)

    cond do
      h == ?- -> [h | Enum.map(tail, &(do_addition(&1)))]
      true -> do_addition(formula)
    end
    |> do_op(?-)
  end
  ## Div then Mul then Add then Sub
  def calculate(formula) do
    do_subtract(formula)
  end
end

StringNBinaries4.calculate('10 + 5') |> IO.inspect() ## must be 15
StringNBinaries4.calculate('10 - 5') |> IO.inspect() ## must be 5
StringNBinaries4.calculate('10 * 5') |> IO.inspect() ## must be 50
StringNBinaries4.calculate('10 / 5') |> IO.inspect() ## must be 2.0
StringNBinaries4.calculate('10 + 5 - 1') |> IO.inspect() ## must be 14
StringNBinaries4.calculate('10 + 5 / 3') |> IO.inspect() ## must be 11.66666
StringNBinaries4.calculate('10 + 5 * 4 / 2') |> IO.inspect() ## must be 20


defmodule UseBinaries do
  def try do
    bx = << 10, 20, 30 >>
    by = << "a", "b", "c" >>
    bz = << 1.5 :: float >>
    << bx :: binary, by :: binary, bz :: binary >> |> IO.inspect()

    ## bit extraction say for pi float
    << sign::size(1), exp::size(11), mantissa::size(52) >> = << 3.14159265::float >>
    (1 + mantissa / :math.pow(2, 52))  * :math.pow(2, exp-1023) * (1 - 2*sign) |> IO.inspect()
  end

  def pat do
    ##
    <<_::1, number::15-integer-little-signed>> = <<1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1, 1::1>>
    IO.puts("> #{number}")

    ## Bin11 n Bin12 binaries of size 3
    bin11 = <<1, 17, 42>>
    bin12 = <<"abc">>
    IO.puts("#{bin11} | #{:erlang.binary_to_list(bin11)}")
    IO.puts("#{bin12} | #{:erlang.binary_to_list(bin12)}")

    bin13 =  <<0, "foo">>
    IO.puts("#{bin13} | #{:erlang.binary_to_list(bin13)}")

    "hełło" <> <<0>> |> IO.inspect()
    IO.inspect("hełło", binaries: :as_binaries)
    <<42>> = <<42::8>> ## ::n modifier to show bit size
    <<0::1, 0::1, 1::1, 1::1>> = <<3::4>> ## decimal 3 represented with 4 bits in base 2
    <<1>> = <<257>> ## value exceeding number of bits provisioned is truncated
    is_bitstring(<<3::4>>) |> IO.inspect()
    is_binary(<<3::4>>) |> IO.inspect()
    is_binary(<<3, 4>>) |> IO.inspect()

    <<0, 1, x::binary>> = <<0, 1, 2, 3>>
    IO.inspect(x)
    <<h1::binary-size(2), r1::binary>> = <<0, 1, 2, 3>>
    IO.puts("> #{h1} | #{r1}")
    <<h2, r2::binary>> = "banana"
    IO.puts("> #{h2} | #{r2}")
    <<x::utf8, r3::binary>> = "über"
    IO.puts("> #{x} | #{r3}")
  end
end

UseBinaries.try()
UseBinaries.pat()


defmodule UseStringModule do
  def try do
    chalice = "câlice"
    String.length(chalice) |> IO.inspect()      ## 6
    byte_size(chalice) |> IO.inspect()          ## 7
    String.at(chalice, 2) |> IO.inspect()       ## l
    String.at(chalice, -2) |> IO.inspect()      ## c
    String.first("warn") |> IO.inspect()        ## "w"
    String.last("warn") |> IO.inspect()         ## "n"
    String.split("the king") |> IO.inspect()    ## ["the", "king"]
    String.split(chalice, "l") |> IO.inspect()  ## ["câ", "ice"]
    String.split("the king", ~r{[hi]}) |> IO.inspect()    ## ["the", "king"]
    String.split("the king", ~r{[hi]}, parts: 2) |> IO.inspect()    ## ["the", "king"]
    String.slice(chalice, 3, 1) |> IO.inspect() ## "i"

    String.capitalize(chalice) |> IO.inspect()  ## Câlice
    String.downcase("Câlice") |> IO.inspect()   ## câlice
    String.upcase("Câlice") |> IO.inspect()     ## CÂLICE
    String.duplicate("|-", 3) |> IO.inspect()   ## |-|-|-
    String.starts_with?("warning", "wa") |> IO.inspect()   ## true
    String.valid?(chalice) |> IO.inspect()          ## true
    String.valid?(chalice) |> IO.inspect()          ## true
    String.valid?(<< 0x77, 0x88 >>) |> IO.inspect() ## false
    String.starts_with?("warning", ["ua", "va", "wa"]) |> IO.inspect()   ## true
    String.ends_with?("warning", ["er", "ed", "ing"]) |> IO.inspect()   ## true
    String.ends_with?("warn", ["er", "ed", "ing"]) |> IO.inspect()   ## false
    String.printable?(chalice) |> IO.inspect()  ## true
    String.printable?("\nchalice") |> IO.inspect()  ## false

    String.jaro_distance(chalice, "chalice") |> IO.inspect()        ## 0.8492063492063492 0to1 similarity ratio
    String.myers_difference(chalice, "chalice") |> IO.inspect()     ## [eq: "c", del: "â", ins: "ha", eq: "lice"] transformations needed to convert

    String.trim("\nmain  ") |> IO.inspect()          ## "main"
    String.trim_leading("\nmain  ") |> IO.inspect()  ## "main  "
    String.trim_trailing("\nmain  ") |> IO.inspect() ## "\nmain"
    String.trim("__main__", "_") |> IO.inspect()          ## "main"
    String.trim_leading("__main__", "_") |> IO.inspect()  ## "main__"
    String.trim_trailing("__main__", "_") |> IO.inspect() ## "__main"
    String.pad_leading(chalice, 10, "_") |> IO.inspect()  ## "____câlice"
    String.pad_trailing(chalice, 10, "_") |> IO.inspect() ## "câlice____"
    String.replace(chalice, "c", "_") |> IO.inspect() ## "_âli_e"
    String.reverse(chalice) |> IO.inspect() ## "ecilâc

    ## codepoints returns all chars separately, graphemes return combined diaresis
    String.codepoints("noi\u0308ce") |> IO.inspect() ## ["n", "o", "i", "̈", "c", "e"]
    String.graphemes("noi\u0308ce") |> IO.inspect()  ## ["n", "o", "ï", "c", "e"]
    ## String.next_codepoint/1 and String.next_graphemes/1 splits str into leading codepoint/graphemes and rest to be used as basis of an iterator
  end
end

UseStringModule.try()



defmodule StringNBinaries5 do
  defp do_center([], _w), do: :ok
  defp do_center([h|tail], width) do
  plus_pad = String.length(h) + ((width - String.length(h))/2) |> trunc()
    String.pad_leading(h, plus_pad)
      |> IO.puts()
    do_center(tail, width)
  end
  def center(lst) do
    width = Enum.map(lst, &(String.length(&1))) |> Enum.max()
    do_center(lst, width)
  end
end

StringNBinaries5.center(["at", "bat", "tabernak", "chat"])


## String Processing with Binaries
defmodule Utf8 do
  def each(str, func) when is_binary(str), do: _each(str, func)
  def each(_s, _f), do: raise "Invalid input."
  defp _each(<< head :: utf8, tail :: binary >>, func) do
    func.(head)
    _each(tail, func)
  end
  defp _each(<<>>, _func), do: []
end
##
Utf8.each "∂og", fn char -> IO.puts char end


## String And Binaries exercise 6 & 7
defmodule StringNBinaries6n7 do
  def capitalize_sentence(sentence) do
    String.split(sentence, ".")
    |> Enum.map(&(String.trim(&1) |> String.capitalize()))
    |> Enum.join(". ")
    |> String.trim()
  end

  defp do_lines_to_records([id, place, cost]) do
    [{:id, id}, {:place, place}, {:cost, cost}]
  end
  def lines_to_records(lines) do
    String.split(lines, "\n")
    |> Enum.map(fn l -> String.split(l, ",") end)
    |> Enum.map(fn x -> Enum.map(x, &(String.trim(&1))) end)
    |> Enum.reject(&(&1 == [""] or &1 == ["id", "place", "cost"]))
    |> Enum.map(&(do_lines_to_records(&1)))
  end
end

StringNBinaries6n7.capitalize_sentence("oh. come ON. play it.") |> IO.inspect()
StringNBinaries6n7.capitalize_sentence("oh. come ON. play") |> IO.inspect()

StringNBinaries6n7.lines_to_records("""
  id,place,cost
  11,:A,1000.00
  12,:C,1400.00
  13,:B, 1030.00
  14,:A,1002.00
  """) |> IO.inspect()
