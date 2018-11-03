%%% location of the server node

%% return hostname
hostname() ->
  {ok, Hostname} = inet:gethostname(),
  Hostname.

-define(server_node, list_to_atom("messenger@" ++ hostname())).
