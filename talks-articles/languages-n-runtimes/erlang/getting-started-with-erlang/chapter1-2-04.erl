-module('chapter1-2-04').
-export([list_len/1]).

list_len([]) ->
  0;
list_len([_ | Rest]) ->
  1 + list_len(Rest).
