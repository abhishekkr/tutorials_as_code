-module('chapter1-2-10').
-export([rev_list/1, format_temps/1]).

%% reverse a given list
rev_list(Lst) ->
  rev_list(Lst, []).

rev_list([First | Rest], Rev_List) ->
  io:format("calling with~n(rest: ~w, [first: ~w, rev-list: ~w])~n", [Rest, First, Rev_List]),
  rev_list(Rest, [First | Rev_List]);
rev_list([], Rev_List) ->
  Rev_List.


%% format_temps to converting all temp from list to celsius
format_temps(City_Temperatures) ->
  Converted_List = convert_list_to_c(City_Temperatures),
  print_temp(Converted_List),
  {Max_city, Min_city} = find_max_and_min(Converted_List),
  print_max_and_min(Max_city, Min_city).

convert_list_to_c([{City, {f, F}} | Rest]) ->
  City_Temperatures = {City, {c, ((F-32)*5 /9)}},
  [City_Temperatures | convert_list_to_c(Rest)];
convert_list_to_c([City_Temperatures | Rest]) ->
  [City_Temperatures | convert_list_to_c(Rest)];
convert_list_to_c([]) ->
  [].

print_temp([{Name, {c, Temp}} | Rest]) ->
  io:format("~-15w ~w c~n", [Name, Temp]),
  print_temp(Rest);
print_temp([]) ->
  ok.

find_max_and_min([City_Temperature | Rest]) ->
  find_max_and_min(Rest, City_Temperature, City_Temperature).

find_max_and_min([{City, {c, C}} | Rest], {City1, {c, C1}}, {City2, {c, C2}}) ->
  Max_City = if C > C1 ->
       {City, {c, C}};
  true ->
       {City1, {c, C1}}
  end,
  if C < C2 ->
       Min_City = {City, {c, C}};
  true ->
       Min_City = {City2, {c, C2}}
  end,
  find_max_and_min(Rest, Max_City, Min_City);
find_max_and_min([], Max_City, Min_City) ->
  {Max_City, Min_City}.

print_max_and_min({Max_name, {c, Max_temp}}, {Min_name, {c, Min_temp}}) ->
  io:format("Max temperature was ~w c in ~w~n", [Max_temp, Max_name]),
  io:format("Min temperature was ~w c in ~w~n", [Min_temp, Min_name]).
