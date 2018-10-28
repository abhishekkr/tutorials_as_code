-module('chapter1-3-02').
-export([start/0, ping/2, pong/0,
        echo/0]).

%% ping getting message from pong
ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("ping finished~n", []);
ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N-1, Pong_PID).

%% pong getting message from ping
pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.


%% echo
echo() ->
  receive
    bye ->
      io:format("; bye~n");
    hola ->
      io:format("; welcome~n"),
      echo();
    Other ->
      io:format("; ~p~n", [Other]),
      echo()
  end.


start() ->
  Pong_PID = spawn('chapter1-3-02', pong, []),
  spawn('chapter1-3-02', ping, [2, Pong_PID]),

  Echo_Pid = spawn(fun() -> echo() end),
  Echo_Pid ! hola,
  Echo_Pid ! "say what",
  Echo_Pid ! "say something",
  Echo_Pid ! bye.
