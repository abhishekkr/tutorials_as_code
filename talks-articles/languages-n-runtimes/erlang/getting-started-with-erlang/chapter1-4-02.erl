-module('chapter1-4-02').

-export([start/1, pingWithLink/2, pong/0,
         jingJong/1, jingWithLink/1, jongWithTrap/0]).

%%% link processes

pingWithLink(N, Pong_PID) ->
  link(Pong_PID), %% here marks pong to exit when pingWithLink exits
  ping(N, Pong_PID).

ping(0, _) ->
  exit(ping);
ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      io:format("ping got pong~n", [])
  end,
  ping(N-1, Pong_PID).

pong() ->
  receive
    {ping, Ping_PID} ->
      io:format("Pong got ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

start(Ping_Node) ->
  Pong_PID = spawn('chapter1-4-02', pong, []),
  spawn(Ping_Node, 'chapter1-4-02', pingWithLink, [3, Pong_PID]).


%%% process_flag to trap exit

jingWithLink(Jong_PID) ->
  link(Jong_PID),
  jing(Jong_PID).
jing(Jong_PID) ->
  Jong_PID ! {jing, self()},
  receive
    jong ->
      io:format("jing...", [])
  end.

jongWithTrap() ->
  process_flag(trap_exit, true),
  jong().
jong() ->
  receive
    {jing, Jing_PID} ->
      io:format("jong...", []),
      Jing_PID ! jong,
      jong();
    {'EXIT', From, Reason} ->
      io:format("jong exits, got ~p~n", [{'EXIT', From, Reason}])
  end.

jingJong(Jing_Node) ->
  Jong_PID = spawn('chapter1-4-02', jongWithTrap, []),
  spawn(Jing_Node, 'chapter1-4-02', jingWithLink, [Jong_PID]).
