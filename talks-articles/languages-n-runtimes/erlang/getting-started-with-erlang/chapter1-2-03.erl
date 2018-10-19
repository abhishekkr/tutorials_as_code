-module('chapter1-2-03').
-export([convert/2, convert_length/1]).

%% converting from inches to centimeter and vice-versa
%% 'chapter1-2-03':convert(5, inch).
%% 'chapter1-2-03':convert(5, centimeter).
convert(M, inch) ->
  M / 2.54;
convert(N, centimeter) ->
  N * 2.54.


convert_length({centimeter, X}) ->
  {inch, X /2.54};
convert_length({inch, X}) ->
  {centimeter, X *2.54}.
