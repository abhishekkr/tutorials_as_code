-module(chapter2_5_1).
-export([print/1, either_or_both/2, either_o_both/2, area/1]).

%% 2.5.1 side effects

print(Term) ->
  io:format("is ~p also ~w~n", [Term, Term]).


%% 2.5.2 multiple clauses

either_or_both(true, _) ->
  true;
either_or_both(_, true) ->
  true;
either_or_both(_, _) ->
  false.


%% 2.5.3 guards

either_o_both(true, B) when is_boolean(B) ->
  true;
either_o_both(A, true) when is_boolean(A) ->
  true;
either_o_both(false, false) ->
  false.


%% patterns, clauses, variable scope
area({circle, Radius}) when is_number(Radius) ->
  Radius * Radius * math:pi();
area({square, Side}) when is_number(Side) ->
  Side * Side;
area({rectangle, Height, Width}) when is_number(Height), is_number(Width) ->
  Height * Width.
