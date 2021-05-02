defmodule Collectibles do
  def enum do
    lsta = Enum.to_list(5..10)
    lstw = ["crazy", "brown", "rabbit", "jumped", "over", "the", "fence"]

    lstb = Enum.to_list(1..4) |> Enum.concat(lsta)

    lstc = Enum.map(lstb, &(&1 * 2))
    strs = Enum.map(lsta, &String.duplicate("+", &1))

    lsta |> IO.inspect()
    lstb |> IO.inspect()
    lstc |> IO.inspect()
    strs |> IO.inspect()

    Enum.at(10..20, 0) |> IO.inspect()
    Enum.at(10..20, 5) |> IO.inspect()
    Enum.at(10..20, 15, :undefined) |> IO.inspect()

    Enum.filter(lsta, &(rem(&1, 2) == 0)) |> IO.inspect()
    Enum.reject(lsta, &(rem(&1, 2) == 0)) |> IO.inspect()

    Enum.sort(lstw) |> IO.inspect()
    Enum.sort(lstw,
      &(String.length(&1) <= String.length(&2))
    ) |> IO.inspect()
    Enum.max(lstw) |> IO.inspect()
    Enum.max_by(lstw, &String.length/1) |> IO.inspect()

    Enum.take(lstw, 3) |> IO.inspect()
    Enum.take_every(lstw, 3) |> IO.inspect()
    Enum.take_while(lstw, &(String.contains?(&1, "r"))) |> IO.inspect()

    Enum.split(lstw, 3) |> IO.inspect()
    Enum.split_while(lstw, &(String.contains?(&1, "r"))) |> IO.inspect()

    Enum.drop_every(lstw, 3) |> IO.inspect()

    Enum.join(lsta) |> IO.inspect()
    Enum.join(lsta, ", ") |> IO.inspect()
    Enum.join(lstw, "|") |> IO.inspect()

    Enum.all?(lsta, &(&1 < 7)) |> IO.inspect()
    Enum.all?(lsta, &(&1 < 17)) |> IO.inspect()
    Enum.any?(lsta, &(&1 < 7)) |> IO.inspect()
    Enum.member?(lsta, 17) |> IO.inspect()
    Enum.empty?(lsta) |> IO.inspect()

    Enum.zip(lsta, [:a, :b, :c]) |> IO.inspect()
    Enum.with_index(lsta) |> IO.inspect()

    ## this gives 3; cuz (10 - (9 - (8 - (7 - (6 - 5)))))
    Enum.reduce(lsta, &(&1 - &2)) |> IO.inspect()
    ## this gives -35; cuz (((((5 - 6) - 7) - 8) - 9) - 10)
    Enum.reduce(lsta, &(&2 - &1)) |> IO.inspect()

    Enum.reduce(lstw, fn wrd, longest ->
      if String.length(wrd) > String.length(longest) do
        wrd
      else
        longest
      end
    end) |> IO.inspect()
    Enum.reduce(lstw, 0, fn wrd, longest ->
      if String.length(wrd) > longest,
      do: String.length(wrd),
      else: longest
      end) |> IO.inspect()

    what_todo = for day <- '1234567', hobby <- [:draw,:walk,:write], do: [day, hobby]
    what_todx = for day <- '1234567', hobby <- 'dwW', do: [day, hobby]
    what_todo |> Enum.shuffle() |> Enum.take(3) |> IO.inspect()
    what_todx |> Enum.shuffle() |> Enum.chunk(7) |> IO.inspect()

    ## can `import Enum` and simply do join(lstw)
  end

  def sort_case do
    ## important to use `<=` for stable result
    Enum.sort(["abcd", "bca", "cdefg"],
      &(String.length(&1) <= String.length(&2))
    ) |> IO.inspect()
    ## following reverses without need
    Enum.sort(["abc", "bca", "cde"],
      &(String.length(&1) < String.length(&2))
    ) |> IO.inspect()
  end
end

Collectibles.enum
Collectibles.sort_case


## exercise to implement Enum like functions
defmodule ListRecursion5n6 do
  def eg_lst, do: [1, 3, 4, 5, 6, 7, 9]

  def all?([], _foo), do: true
  def all?([h | tail], foo) do
    if foo.(h), do: all?(tail, foo), else: false
  end

  def each([], _foo), do: :ok
  def each([h | tail], foo) do
    foo.(h)
    each(tail, foo)
  end

  def filter([], _foo), do: []
  def filter([h | tail], foo) do
    if foo.(h), do: [h | filter(tail, foo)], else: filter(tail, foo)
  end

  defp do_split([h|tail], frst, count, idx) when idx < count, do: do_split(tail, frst ++ [h], count, idx+1)
  defp do_split(lst, frst, count, idx) when idx >= count, do: {frst, lst}
  def split([], _count), do: {[], []}
  def split(lst, 0), do: {[], lst}
  def split(lst, count) when count < 0, do: split(lst, length(lst)+count)
  def split([h|tail], count), do: do_split(tail, [h], count, 1)

  def take(lst, count) when count == 0, do: []
  def take(lst, count) when count > 0 do
    {result, _} = split(lst, count)
    result
  end
  def take(lst, count) when count < 0 do
    {_, result} = split(lst, count)
    result
  end

  def do_flatten([h|tail], xtail), do: do_flatten(h, tail) ++ flatten(xtail)
  def do_flatten(h, xtail), do: [h | flatten(xtail)]
  def flatten([]), do: []
  def flatten([h|tail]), do: do_flatten(h, tail)
end

ListRecursion5n6.eg_lst |> ListRecursion5n6.all?(&(&1 < 10)) |> IO.inspect()
ListRecursion5n6.eg_lst |> ListRecursion5n6.all?(&(rem(&1, 2) == 0)) |> IO.inspect()

ListRecursion5n6.eg_lst |> ListRecursion5n6.each(&(IO.inspect(&1 + 10))) |> IO.inspect()

ListRecursion5n6.eg_lst |> ListRecursion5n6.filter(&(rem(&1, 2) == 0)) |> IO.inspect()

[] |> ListRecursion5n6.split(3) |> IO.inspect()
ListRecursion5n6.eg_lst |> ListRecursion5n6.split(3) |> IO.inspect()
ListRecursion5n6.eg_lst |> ListRecursion5n6.split(0) |> IO.inspect()
ListRecursion5n6.eg_lst |> ListRecursion5n6.split(-2) |> IO.inspect(charlists: :as_lists)
ListRecursion5n6.eg_lst |> ListRecursion5n6.take(2) |> IO.inspect(charlists: :as_lists)
ListRecursion5n6.eg_lst |> ListRecursion5n6.take(-2) |> IO.inspect(charlists: :as_lists)

ListRecursion5n6.flatten([[[1,2],3],4,[5,6]]) |> IO.inspect(charlists: :as_lists)
ListRecursion5n6.flatten([[[1,2],3],4,5,6]) |> IO.inspect(charlists: :as_lists)
ListRecursion5n6.flatten([1,2,3,4,[5,6]]) |> IO.inspect(charlists: :as_lists)
ListRecursion5n6.flatten([1,2,3,4,5,6]) |> IO.inspect(charlists: :as_lists)


## Streams- Lazy Enumerables
defmodule UseStreams do
  def try do
    s = Stream.map([1,2,3,5], &(&1*10))
    Enum.to_list(s) |> IO.inspect()

    ## can pass a stream to a stream
    sqrs = Stream.map(1..7, &(&1*&1))
    Enum.to_list(sqrs) |> IO.inspect()
    pls1 = Stream.map(sqrs, &(&1+1))
    Enum.to_list(pls1) |> IO.inspect()
    odds = Stream.filter pls1, fn x -> rem(x,2) == 1 end
    Enum.to_list(odds) |> IO.inspect()
  end

  def vs_enum do
    IO.puts("Enum.map(1..1_000_000, &(&1+1)) |> Enum.take(5)")
    #Enum.map(1..1_000_000_000, &(&1+1)) |> Enum.take(5)

    IO.puts("Stream.map(1..1_000_000, &(&1+1)) |> Enum.take(5)")
    Stream.map(1..1_000_000_000, &(&1+1)) |> Enum.take(5) |> IO.inspect()
  end

  def wrap do
    Stream.cycle(~w{this that})
      |> Stream.zip(~w{left right front back})
      |> Enum.map(fn {tl,dr} -> "#{tl} goes #{dr}" end)
      |> IO.inspect()

    Stream.repeatedly(fn -> DateTime.utc_now() end)
      |> Enum.take(3)
      |> IO.inspect()

    Stream.iterate([DateTime.utc_now()], fn t -> [DateTime.utc_now() | t] end)
      |> Enum.take(3)
      |> IO.inspect()

    Stream.unfold({0, 1}, fn {strm, nu} -> {strm, {nu, strm+nu}} end)
      |> Enum.take(7) |> IO.inspect()
    Stream.unfold({0, 1}, fn {strm, nu} -> {strm, {strm+nu, nu}} end)
      |> Enum.take(5) |> IO.inspect()
    Stream.unfold({true, false}, fn {a, b} -> {a, {b, a}} end)
      |> Enum.take(7) |> IO.inspect()
  end

  ## Collectables & Comprehensions
  def cc do
    ## Collectables
    Enum.into(1..5, []) |> IO.inspect()
    Enum.into([a: 1, b: 2], %{}) |> IO.inspect()

    ##DEPRECATED## Enum.into(1..5, [10, 20]) |> IO.inspect()
    Enum.to_list(1..5) ++ [10,20] |> IO.inspect()
    Keyword.merge([a: 1, b: 2], [a: 3, d: 4]) |> IO.inspect()

    ## Comprehensions
    lsta = for idx <- [10,21,30], rem(idx,2) == 0, do: idx*idx
    lsta |> IO.inspect()
    ### later generator act as nested
    lstb = for idx <- [10,20,30], idy <- [1,10], do: idx*idy
    lstb |> IO.inspect()
    ### can use generator in inner generator
    lstc = for {min,max} <- [{1,4},{2,3},{5,7}], n <- min..max, do: n
    lstc |> IO.inspect()
    ### FILTERS
    lstd = for x <- lsta, y <- lstb, x > y, rem(x,y) == 0, do: {x, y}
    lstd |> IO.inspect()
    ## swappy
    lste = for {a,b} <- [a: :ok, b: :warn, c: :err], do: {b, a}
    lste |> IO.inspect()
    ## used on Bits
    (for << c <- "hell-o" >>, do: c) |> IO.inspect(charlists: :as_lists)
    (for << c <- "hell-o" >>, do: c) |> IO.inspect()
    (for << c <- "hell-o" >>, do: <<c>>) |> IO.inspect() ## converting back to string
    ## using pattern with BITS
    (for << << b1::size(2), b2::size(3), b3::size(4) >> <- "hell-o" >>, do: "#{b1}-#{b2}-#{b3}") |> IO.inspect() ## converting back to string
    (for << << b1::size(4) >> <- "h" >>, do: "#{b1}") |> IO.inspect() ## ["6", "8"]

    ## returning into
    (for x <- ~w{apple ball cat}, into: %{}, do: {x, String.length(x)}) |> IO.inspect()
    (for x <- ~w{apple ball cat}, into: Map.new, do: {x, String.length(x)}) |> IO.inspect()
    (for x <- ~w{apple ball cat}, into: %{"dog" => 3}, do: {x, String.length(x)}) |> IO.inspect()
  end
end

UseStreams.try
UseStreams.vs_enum
UseStreams.wrap
UseStreams.cc


## example usage of Stream.resource
defmodule TickTock do
  def sleep(sec) do
    receive do
      after sec*1000 -> nil
    end
  end

  def ticker(txt) do
    spawn fn -> IO.puts(txt) end
  end

  def timer do
    Stream.resource(
      fn ->
        {_hr, _min, sec} = :erlang.time
        IO.inspect({sec, 60 - sec - 1})
        60 - sec - 1
      end,
      fn
        0 ->
          {:halt, 0}
        counter ->
          sleep(1)
          { [ticker(counter)], counter-1 }
      end,
      fn _ -> nil end
    )
  end
end

TickTock.timer |> Enum.take(5)


## exercise ListRecursion7n8
defmodule ListRecursion7n8 do
  ## from chapter.07
  defp do_span(start, fin, lst) when start > fin, do: lst
  defp do_span(start, fin, lst), do: do_span(start+1, fin, lst ++ [start])
  defp span(start, fin), do: do_span(start, fin, [])

  defp is_prime(2), do: true
  defp is_prime(num) do
    Enum.all?(
      span(2, (num/2)+1),
      &(rem(num,&1) != 0))
  end
  def prime_to(range) do
    for num <- span(2, range),
      is_prime(num), do: num
  end

  ## rates being a keyword list of PLACE's Tax; orders %{id,PLACE,price}
  def apply_tax() do
    tax_percents = [a: 2, b: 3]
    orders = [
      [id: 1, place: :a, cost: 1],
      [id: 2, place: :b, cost: 3],
      [id: 3, place: :b, cost: 4]]

    for o <- orders, t <- [Keyword.get(tax_percents, Keyword.get(o, :place))] do
      cost = Keyword.get(o, :cost)
      o ++ [selling_price: (cost + (cost*t/100))]
    end
  end
end
ListRecursion7n8.prime_to(5) |> IO.inspect()
ListRecursion7n8.prime_to(50) |> IO.inspect()
ListRecursion7n8.apply_tax() |> IO.inspect()
