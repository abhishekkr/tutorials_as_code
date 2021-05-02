## complex list patterns
defmodule MultiMatch do
  def swap_pairs([]), do: []
  def swap_pairs([a, b | tail]), do: [b, a | swap_pairs(tail)]
  def swap_pairs([_|_]), do: raise "Odd element lists are not supported."
end
IO.inspect MultiMatch.swap_pairs([1, 3, 5, 7])
#IO.inspect MultiMatch.swap_pairs([1, 3, 5, 7, 9])

defmodule UserFilter do
  def for_user_7([]),                            do: []
  def for_user_7([[time, 7, host, axn] | tail]), do: [[time, 7, host, axn] | for_user_7(tail)]
  def for_user_7([_ | tail]),                    do: for_user_7(tail)

  def for_user([], _),                                      do: []
  def for_user([[time, userid, host, axn] | tail], userid), do: [[time, userid, host, axn] | for_user(tail, userid)]
  def for_user([_ | tail], userid),                         do: for_user(tail, userid)

  def for([], _),                                      do: []
  def for([ head = [_, userid, _, _] | tail], userid), do: [head | for_user(tail, userid)]
  def for([_ | tail], userid),                         do: for_user(tail, userid)
end

defmodule TestData do
  def datax do
    [
      [1378214512, 7, "svr1", "login"],
      [1378214512, 4, "svr7", "login"],
      [1378214513, 2, "svr3", "login"],
      [1378214513, 7, "svr1", "uptime"],
      [1378214513, 1, "svr1", "logout"],
      [1378214515, 7, "svr1", "logout"],
      [1378214517, 2, "svr1", "logout"],
    ]
  end
end
IO.inspect UserFilter.for_user_7(TestData.datax)
IO.inspect UserFilter.for_user(TestData.datax, 1)
IO.inspect UserFilter.for(TestData.datax, 2)


## list module in action
defmodule ListModule do
  def partx do
    [1, 3] ++ [5, 7, 9] |> IO.inspect

    UserFilter.for(TestData.datax, 2) |> List.flatten |> IO.inspect

    List.foldl([1, 3, 5], "", &("#{&1}_#{&2}")) |> IO.inspect

    List.foldr([1, 3, 5], "", &("#{&1}_#{&2}")) |> IO.inspect

    List.replace_at([1, 3, 5], 1, "wazou") |> IO.inspect
  end

  def party do
    kw = [{:fname, "James"}, {:lname, "Bond"}, {:license, "to-kill", "or-not"}]

    kw |> List.keyfind("to-kill", 1) |> IO.inspect
    kw |> List.keyfind("to-kill", 2) |> IO.inspect
    kw |> List.keyfind("to-kill", 2, "don't kill") |> IO.inspect
    kw |> List.keyfind("or-not", 2) |> IO.inspect
    kw |> List.keydelete("or-not", 2) |> IO.inspect
    kw |> List.keyreplace(:fname, 0, {:first_name, "Jane"}) |> IO.inspect
  end
end
ListModule.partx
ListModule.party


## exercise ListRecursion1to4
defmodule ListRecursion1to4 do
  defp do_mapsum(result, [], _foo), do: result
  defp do_mapsum(result, [h|tail], foo), do: do_mapsum(result+h, tail, foo)
  def mapsum(lst, foo), do: do_mapsum(0, lst, foo)

  def do_max([], m), do: m
  def do_max([h|tail], m) do
    cond do
      h > m ->
        do_max(tail, h)
      true ->
        do_max(tail, m)
    end
  end
  def max([]), do: nil
  def max([h|tail]), do: do_max(tail, h)

  def caeser([], _n), do: []
  def caeser([h|tail], n) when (h+n) > 'z', do: ['?' | caeser(tail, n)]
  def caeser([h|tail], n), do: [h+n | caeser(tail, n)]

  def do_span(start, fin, lst) when start > fin, do: lst
  def do_span(start, fin, lst), do: do_span(start+1, fin, lst ++ [start])
  def span(start, fin), do: do_span(start, fin, [])
end

ListRecursion1to4.mapsum([1,3,4], &(&1+1)) |> IO.inspect()
ListRecursion1to4.max([10,3,4]) |> IO.inspect()
ListRecursion1to4.max([1,3,4]) |> IO.inspect()
ListRecursion1to4.caeser('hell-o', 10) |> IO.inspect()
ListRecursion1to4.caeser('hell-o', 18) |> IO.inspect()
ListRecursion1to4.span(15, 18) |> IO.inspect()
ListRecursion1to4.span(15, 15) |> IO.inspect()
ListRecursion1to4.span(18, 15) |> IO.inspect()
