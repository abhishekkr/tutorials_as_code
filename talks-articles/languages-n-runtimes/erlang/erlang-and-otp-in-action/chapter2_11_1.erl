-module(chapter2_11_1).
-export([show_users/0, update_user/0]).

-record(user, {name="<johndoe>", address, phone}).

update_user() ->
  X = #user{name="Max Payne", address="Town Square", phone="09234521"},
  print_user(X),
  X1 = X#user{name="Nathan Drake"},
  print_user(X1).

show_users() ->
  A = #user{},
  print_user(A),
  B = #user{phone="019283"},
  print_user(B),
  C = #user{name="Max Payne", address="Town Square", phone="09234521"},
  print_user(C).

print_user(#user{name=Name, address=Addr, phone=Phone}) %% fields pattern matched to Variables
    when Phone =/= undefined ->
  io:format("Contact: ~s at ~s. Lives at ~s.~n", [Name, Phone, Addr]);
print_user(X) -> %% if not given an unhandled clause error will be raised for Phone =:= undefined
  fmt_user(X).

fmt_user(X) ->
  io:format("name: ~p, address: ~p, phone: ~p~n",
            [X#user.name, X#user.address, X#user.phone]).
