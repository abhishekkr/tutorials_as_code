-module('chapter1-2-u14').
-export([new/4, blend/2]).

-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
                  ?is_channel(B), ?is_channel(A) ->
  #{red => R, green => G, blue => B, alpha => A}.

blend(Color1,Color2) ->
  blend(Color1, Color2, alpha(Color1, Color2)).

blend(Color1,Color2,Alpha) when Alpha > 0.0 ->
  Color2#{
    red   := red(Color1,Color2) / Alpha,
    green := green(Color1,Color2) / Alpha,
    blue  := blue(Color1,Color2) / Alpha,
    alpha := Alpha
   };
blend(_,Color2,_) ->
  Color2#{
    red   := 0.0,
    green := 0.0,
    blue  := 0.0,
    alpha := 0.0
   }.

alpha(#{alpha := A1}, #{alpha := A2}) ->
  A1 + A2*(1.0 - A1).

red(#{red := R1, alpha := A1}, #{red := R2, alpha := A2}) ->
  R1*A1 + R2*A2*(1.0 - A1).

green(#{green := G1, alpha := A1}, #{green := G2, alpha := A2}) ->
  G1*A1 + G2*A2*(1.0 - A1).

blue(#{blue := B1, alpha := A1}, #{blue := B2, alpha := A2}) ->
  B1*A1 + B2*A2*(1.0 - A1).
