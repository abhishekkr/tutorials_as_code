-module('chapter1-3-01').
-export([start/0, say_something/2]).
-compile([{nowarn_unused_function, [{ say_what,1 }]}]).

say_what(What) ->
  io:format("~p~n", [What]).

say_something(_, 0) ->
  done;
say_something(What, Times) ->
  say_what(What),
  say_something(What, Times - 1).


start() ->
  spawn('chapter1-3-01', say_something, [hello, 2]),
  spawn('chapter1-3-01', say_something, [hola, 1]).
