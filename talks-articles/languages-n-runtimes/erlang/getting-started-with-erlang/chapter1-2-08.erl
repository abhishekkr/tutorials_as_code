-module('chapter1-2-08').
-export([format_temps/1]).

%% only exported functions
format_temps([]) ->  % no output on empty list
  ok;
format_temps([City | Rest]) ->
  print_temp(convert_to_celsius(City)),
  format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) -> % no conversion when celsius atom c
  {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->
  {Name, {c, ((Temp - 32) * 5 / 9)}}.

print_temp({Name, {c, Temp}}) ->
  io:format("~-15w ~w c~n", [Name, Temp]).
