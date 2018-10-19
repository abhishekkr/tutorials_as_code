-module('chapter1-2-09').
-export([max_of_list/1, fst_snd_rest/2]).

max_of_list([Head|Rest]) ->
  max_of_list(Rest, Head).

max_of_list([], Result_so_far) ->
  io:format("result is: ~w~n", [Result_so_far]),
  Result_so_far;
max_of_list([First|Rest], Result_so_far) when First > Result_so_far ->
  io:format("first is greater than results so far~nfirst: ~w, rest: ~w~n", [First, Rest]),
  max_of_list(Rest, First); % will call this with first as Result_so_far
max_of_list([_|Rest], Result_so_far) ->
  io:format("first is not greater than results so far~nfirst: doesn't matter, rest: ~w~n", [Rest]),
  max_of_list(Rest, Result_so_far). % will call this with previous Result_so_far

fst_snd_rest([Fst, Snd | Rst], Msg) ->
  io:format("~p~nfirst: ~w, second: ~w, rest: ~w~n", [Msg, Fst, Snd, Rst]).
