-module('chapter1-2-13').
-export([foreach/2, map/2, convert_list_to_c/1]).


foreach(Fn, [First|Rest]) ->
  Fn(First),
  foreach(Fn, Rest);
foreach(_, []) ->
  ok.
%% 'chapter1-2-13':foreach(fun(X) -> X+X end, [1,2,3,4,5])


map(Fn, [First|Rest]) ->
  [Fn(First) | map(Fn, Rest)];
map(_, []) ->
  [].


%% convert
convert_list_to_c(Lst) ->
  New_Lst = lists:map(fun convert_to_c/1, Lst),
  lists:sort(fun({_, {c, Temperature1}}, {_, {c, Temperature2}}) ->
    Temperature1 < Temperature2 end, New_Lst).

convert_to_c({Name, {f, Temperature}}) ->
  {Name, {c, trunc((Temperature - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temperature}}) ->
  {Name, {c, Temperature}}.
