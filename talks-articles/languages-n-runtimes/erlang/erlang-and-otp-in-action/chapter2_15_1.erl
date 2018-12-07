-module(chapter2_15_1).
-export([sum/1, do_sum/1, rev/1, tailrev/1]).

sum(0) -> 0;
sum(N) -> sum(N-1) + N.

do_sum(N) -> do_sum(N, 0).

do_sum(N, Total) when N =/= 0 -> do_sum(N-1, Total+N);
do_sum(0, Total) -> Total.


rev([A | RestOfList]) -> rev(RestOfList) ++ [A];
rev([]) -> [].


tailrev(List) -> tailrev(List, []).

tailrev([X | RestOfList], Acc) -> tailrev(RestOfList, [X | Acc]);
tailrev([], Acc) -> Acc.
