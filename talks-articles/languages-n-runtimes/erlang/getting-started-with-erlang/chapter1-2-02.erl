-module('chapter1-2-02').
-export([factorial/1, plusplus10/2]).

factorial(1) ->
  1;
factorial(N) ->
  N * factorial(N-1).

%% 'chapter1-2-02':plusplus10(1, 99). gives 110
plusplus10(X, Y) ->
  X + Y + 10.
