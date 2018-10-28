-module('chapter1-3-04').

-export([start/1, start_ping/1, start_pong/0, ping/2, pong/0]).

ping(0, Pong_Node) ->
  {pong, Pong_Node} ! finished,
  io:format("ping finished~n", []);
ping(N, Pong_Node) ->
  {pong, Pong_Node} ! {ping, self()},
  receive
    pong ->
      io:format("ping got pong~n", [])
  end,
  ping(N-1, Pong_Node).

pong() ->
  receive
    {ping, Ping_PID} ->
      io:format("pong got ping~n", []),
      Ping_PID ! pong,
      pong();
    finished ->
      io:format("pong finished~n", [])
  end.


start_pong() ->
  register(pong, spawn('chapter1-3-04', pong, [])).

start_ping(Pong_Node) ->
  spawn('chapter1-3-04', ping, [3, Pong_Node]).

start(Ping_Node) ->
  register(pong, spawn('chapter1-3-04', pong, [])),
  spawn(Ping_Node, 'chapter1-3-04', ping, [3, node()]).
