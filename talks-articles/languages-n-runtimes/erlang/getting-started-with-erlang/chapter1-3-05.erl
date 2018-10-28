%%%% # Chat now
%%%%
%%%% ## User Interface:
%%%% * logon(Name)
%%%%      One user at a time can log in from each erlang node in messenger
%%%%      and choose a suitable Name.
%%%%      If Name is already logged or node logged from, login is rejected.
%%%% * logoff()
%%%%      Logs off anybody at that node.
%%%% * message(ToName, Message)
%%%%      Sends Message to ToName.
%%%%
%%%%  One node in network of Erlang nodes run a server which maintains data.
%%%%  Server is registered as 'messenger', client runs registered as `mess_client`
%%%%
%%%%  ---
%%%%
%%%% ## Protocols between client processes and server
%%%%
%%%% To server: {ClientPid, logon, UserName}
%%%% Reply {messenger, stop, user_exists_at_other_node} stops the client
%%%% Reply {messenger, logged_on} logon was successful
%%%%
%%%% To server: {ClientPid, logoff}
%%%% Reply: {messenger, logged_off}
%%%%
%%%% To server: {ClientPid, logoff}
%%%% Reply: no reply
%%%%
%%%% To server: {ClientPid, message_to, ToName, Message} send a message
%%%% Reply: {messenger, stop, you_are_not_logged_on} stops the client
%%%% Reply: {messenger, receiver_not_found} no user with this name logged on
%%%% Reply: {messenger, sent} Message has been sent (but no guarantee)
%%%%
%%%% To client: {message_from, Name, Message},
%%%%
%%%% ---
%%%%
%%%% ## Protocol between the "commands" and the client
%%%%
%%%% Started: messenger:client(Server_Node, Name)
%%%% To client: logoff
%%%% To client: {message_to, ToName, Message}
%%%%
%%%% Configuration: change the server_node() function to return
%%%%   name of node where the messenger server runs.
%%%%
%%%% ---

-module('chapter1-3-05').

-export([start_server/0, server/1,
         logon/1, logoff/0, message/2,
         client/2]).

%%%% server functions

%% return hostname
hostname() ->
  {ok, Hostname} = inet:gethostname(),
  Hostname.

%% returns name of node where server runs
server_node() ->
  list_to_atom("messenger@" ++ hostname()).

%% server process "messenger", user list has format [{ClientPid1, Name1},...]
server(User_List) ->
  receive
    {From, logon, Name} ->
      New_User_List = server_logon(From, Name, User_List),
      server(New_User_List);
    {From, logoff} ->
      New_User_List = server_logoff(From, User_List),
      server(New_User_List);
    {From, message_to, To, Message} ->
      server_transfer(From, To, Message, User_List),
      io:format("list is now: ~p~n", [User_List]),
      server(User_List)
  end.

%% updates server's user list if the user is new, else returns error
server_logon(From, Name, User_List) ->
  case lists:keymember(Name, 2, User_List) of
    true ->
      From ! {messenger, stop, user_exists_at_other_node}, % reject logon
      User_List;
    false ->
      From ! {messenger, logged_on},
      [{From, Name} | User_List] % add user to logon list
  end.

%% removes user from server's user list
server_logoff(From, User_List) ->
  lists:keydelete(From, 1, User_List).

%% return error from server_transfer if missing user,
%% else calls a func which transfers message
server_transfer(From, To, Message, User_List) ->
  case lists:keysearch(From, 1, User_List) of
    false ->
      From ! {messenger, stop, you_are_not_logged_on};
    {value, {From, Name}} ->
      server_transfer(From, Name, To, Message, User_List)
  end.
%% if user exists send the message
server_transfer(From, Name, To, Message, User_List) ->
  case lists:keysearch(To, 2, User_List) of
    false ->
      From ! {messenger, receiver_not_found};
    {value, {ToPid, To}} ->
      ToPid ! {message_from, Name, Message},
      From ! {messenger, sent}
  end.


%% start messaging server
start_server() ->
  register(messenger, spawn('chapter1-3-05', server, [[]])).


%% User commands
logon(Name) ->
  case whereis(mess_client) of
    undefined ->
      register(mess_client,
               spawn('chapter1-3-05', client, [server_node(), Name]));
    _ ->
      already_logged_on
  end.

%% logoff a messenger client
logoff() ->
  mess_client ! logoff.

%% sending message from a name
message(ToName, Message) ->
  case whereis(mess_client) of
    undefined ->
      not_logged_on;
    _ ->
      mess_client ! {message_to, ToName, Message},
      ok
  end.


%% client functions

client(Server_Node, Name) ->
  {messenger, Server_Node} ! {self(), logon, Name},
  await_result(),
  client(Server_Node).

client(Server_Node) ->
  receive
    logoff ->
      {messenger, Server_Node} ! {self(), logoff},
      exit(normal);
    {message_to, ToName, Message} ->
      {messenger, Server_Node} ! {self(), message_to, ToName, Message},
      await_result();
    {message_from, FromName, Message} ->
      io:format("@~p: ~p~n", [FromName, Message])
  end,
  client(Server_Node).


%% wait for a response
await_result() ->
  receive
    {messenger, stop, Why} ->
      io:format("(bye!) ~p~n", [Why]),
      exit(normal);
    {messenger, What} ->
      io:format("~p~n", [What])
  end.
