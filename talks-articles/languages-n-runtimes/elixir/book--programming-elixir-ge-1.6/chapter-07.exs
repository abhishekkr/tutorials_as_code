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
