-module('chapter1-3-03').
-export([start/0, ping/1, pong/0,
        bob/0, eve/0]).

%% re-writing ping-pong with register
ping(0) ->
  pong_pid ! finished,
  io:format("ping finished~n", []);
ping(N) ->
  pong_pid ! {ping, self()},
  receive
    pong ->
      io:format("ping got pong~n", [])
  end,
  ping(N-1).

pong() ->
  receive
    {ping, Ping_PID} ->
      Ping_PID ! pong,
      pong();
    finished ->
      io:format("no more pong~n")
  end.


%% p2p chatting spawns using register
bob() ->
  receive
    {_, "bye"} ->
      io:format("bob left the channel~n");
    {From, Msg} ->
      io:format("@~p: what is ~p~n", [From, Msg]),
      bob()
  end.

eve() ->
  receive
    {_, "bye"} ->
      io:format("eve left the channel~n");
    {From, Msg} ->
      io:format("@~p: what is ~p~n", [From, Msg]),
      eve()
  end.

start() ->
  register(pong_pid, spawn('chapter1-3-03', pong, [])),
  spawn('chapter1-3-03', ping, [2]),
  register(bob_pid, spawn('chapter1-3-03', bob, [])),
  register(eve_pid, spawn('chapter1-3-03', eve, [])),
  bob_pid ! {"eve", "hola"},
  eve_pid ! {"bob", "ahoy"},
  bob_pid ! {"eve", "bye"},
  eve_pid ! {"bob", "bye"}.

