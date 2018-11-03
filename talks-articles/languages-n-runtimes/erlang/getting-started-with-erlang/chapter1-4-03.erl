-module('chapter1-4-03').

-export([start_server/0, server/0,
         logon/1, logoff/0, message/2, client/2]).

%%%%
%%% One node in erlang nodes' network runs server,
%%%   it maintains data about logged in users.
%%%   Server is registered as 'messenger'.
%%% Each node with user gets logged on as client process
%%%   registered as 'mess_client'
%%%% ---


%%%% server functions
%% return hostname
hostname() ->
  {ok, Hostname} = inet:gethostname(),
  Hostname.

%% returns name of node where server runs
server_node() ->
  list_to_atom("messenger@" ++ hostname()).


%% server process for messenger
server() ->
  process_flag(trap_exit, true),
  server([]).

server(User_List) ->
  receive
    {From, logon, Name} ->
      New_User_List = server_logon(From, Name, User_List),
      server(New_User_List);
    {'EXIT', From, _} ->
      New_User_List = server_logoff(From, User_List),
      server(New_User_List);
    {From, message_to, To, Message} ->
      server_transfer(From, To, Message, User_List),
      io:format("list is now: ~p~n", [User_List]),
      server(User_List)
  end.


%% adds new user to list
server_logon(From, Name, User_List) ->
  %% check if logged on anywhere else
  case lists:keymember(Name, 2, User_List) of
    true ->
      From ! {messenger, stop, user_exists_at_other_node},
      User_List;
    false ->
      From ! {messenger, logged_on},
      link(From),
      [{From, Name} | User_List]
  end.


%% deletes user from list
server_logoff(From, User_List) ->
  lists:keydelete(From, 1, User_List).


%% transfer message between user
server_transfer(From, To, Message, User_List) ->
  case lists:keysearch(From, 1, User_List) of
    false ->
      From ! {messenger, stop, you_are_not_logged_on};
    {value, {_, Name}} ->
      server_transfer(From, Name, To, Message, User_List)
  end.

server_transfer(From, Name, To, Message, User_List) ->
  case lists:keysearch(To, 2, User_List) of
    false ->
      From ! {messenger, receiver_not_found};
    {value, {ToPid, To}} ->
      ToPid ! {message_from, Name, Message},
      From ! {messenger, sent}
  end.


%%%% user functions
logon(Name) ->
  case whereis(mess_client) of
    undefined ->
      register(mess_client,
              spawn('chapter1-4-03', client, [server_node(), Name]));
    _ -> already_logged_on
  end.

logoff() ->
  mess_client ! logoff.

message(ToName, Message) ->
  case whereis(mess_client) of
    undefined -> not_logged_on;
    _ ->
      mess_client ! {message_to, ToName, Message},
      ok
  end.


%%%% client process
client(Server_Node, Name) ->
  {messenger, Server_Node} ! {self(), logon, Name},
  await_result(),
  client(Server_Node).

client(Server_Node) ->
  receive
    logoff ->
      exit(normal);
    {message_to, ToName, Message} ->
      {messenger, Server_Node} ! {self(), message_to, ToName, Message},
      await_result();
    {message_from, FromName, Message} ->
      io:format("message from @~p: ~p~n", [FromName, Message])
  end,
  client(Server_Node).


%%%% wait for response from server
await_result() ->
  receive
    {messenger, stop, Why} -> % stop client
      io:format("~p~n", [Why]),
      exit(normal);
    {messenger, What} -> % Normal request
      io:format("~p~n", [What])
  after 30000 ->
    io:format("no response from server~n", []),
    exit(timeout)
  end.


%% start the server
start_server() ->
  io:format("server node: ~p~n", [server_node()]),
  register(messenger,
           spawn('chapter1-4-03', server, [])).
