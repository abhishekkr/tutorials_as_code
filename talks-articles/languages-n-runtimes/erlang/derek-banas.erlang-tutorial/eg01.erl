%% @author whoever
%% @doc @todo some description here

-module(eg01).

-import (string, [len/1, concat/2, chr/2, substr/3, str/2,
                  to_lower/1, to_upper/1]).

-export([main/0, ehlo/0, ehlo_add/2]).

%% macro
-define(SINGLE_NAME, "BlahTest").
-define(times3(X), X*3).

main() ->
  ehlo(),
  var_stuff(),
  do_math(10),
  do_random(),
  do_compare(10, 10.0),
  vote(32),
  sum_lst([1,3,5]),
  find_factorial(5),
  find_factorial_tr(5),
  ffor(10, 5),
  map_stuff(),
  record_stuff(),
  hof_stuff(),
  rw_file(),
  error_stuff(),
  spawner().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function declaration, print to stdout
ehlo_add(A, B) ->
  ehlo(),
  A + B.

ehlo() ->
  io:format("~p~n", [say_hey(french)]),
  io:format("~p ~p~n", [?SINGLE_NAME, ?times3(3)]), %% using pre-def macros
  io:fwrite("EHLO!!\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variable, atom, string/tuple/lists tasks, arithmetic operators
var_stuff() ->
  atom_stuff(),
  atom_stufff(),
  string_stuff(),
  tuple_stuff(),
  list_stuff(),
  type_checkr(),
  type_conv(),
  Num = 1,
  Num.

atom_stuff() ->
  an_atom.

atom_stufff() ->
  'Weird Atom'.

string_stuff() ->
  Sa = "Some String",
  Sb = "Other String",
  io:fwrite("Strings: ~p \\= ~p\n", [Sa, Sb]),
  Sc = io_lib:format("~s \\= ~s\n", [Sa, Sb]),
  io:fwrite(Sc),
  Sd = concat("abc", "xyz"),
  io:fwrite("~s is of ~w size\n", [Sd, len(Sd)]),
  io:fwrite("~s ascii is ~s\n", [[$a, $a], [97, 97]]),
  XIndex = chr(Sd, $x),
  Xyz = substr(Sd, XIndex, 2),
  XyzIndex = str(Sd, Xyz),
  %% below ++ concat works as string here is a list of characters
  io:fwrite("~s ~w\n", [to_lower(Xyz) ++ to_upper("Abc"), XyzIndex]).

tuple_stuff() ->
  Tpl = {mydata, 100, "Johnny"},
  {DType, DId, DName} = Tpl,
  {_,DUuid,_} = Tpl,
  {mydata, Uid, FName} = Tpl,
  io:fwrite("~w ~w ~s ~w ~w ~s\n", [DType, DId, DName, DUuid, Uid, FName]).

list_stuff() ->
  La = [10, 11, 12],
  Lb = [10, 20, 30],
  Lc = La ++ Lb,
  Ld = La -- Lb,
  io:fwrite("~w ~w\n", [Lc, Ld]),
  Lhead = hd(Lc),
  Ltail = tl(Lc),
  io:fwrite("~w ~w\n", [Lhead, Ltail]),
  Le = [1|Lc],
  [Lh|Lt] = Le,
  io:fwrite("~w ~w ~w\n", [Le, Lh, Lt]),
  list_comprehension_stuff(Le).

list_comprehension_stuff(Lz) ->
  LzDouble = [2*N || N <- Lz],
  io:fwrite("~w ~w\n", [Lz, LzDouble]),
  LzOddDouble = [2*N || N <- Lz, N rem 2 /= 0],
  io:fwrite("~w\n", [LzOddDouble]),
  CelCity = [{cityX, 11}, {'city y', 21}, {city_z, 31}],
  SelCelCity = [{City, Cel} || {City, Cel} <- CelCity, Cel > 25],
  io:fwrite("city list with above 25 temperature: ~w\n", SelCelCity).

type_checkr() -> %% is_<type>
  is_atom(an_atom) and
  is_float(1.01) and
  is_integer(11) and
  is_boolean(false) and
  is_list([1,2,12]) and
  is_tuple({a, 12}).

type_conv() -> %% <type>_to_<type>
  io:fwrite("~w | ~w | ~w | ~s\n", [
    atom_to_list(abc), list_to_binary("abc"),
    tuple_to_list({abc, 123}), integer_to_list(100)
    ]).

do_math(A) ->
  B = A + A,
  C = B - A,
  D = C * B,
  E = D div 7, %% integer division
  %% underscore used for do-not-care values
  _ = E / 7, %% floating point division
  G = D rem E, %% remainder of division
  H = math:exp(1), %% e raise to
  I = math:log(H),
  J = math:log10(G * 100),
  K = math:pow(I, J),
  math:sqrt(K).

do_random() ->
  rand:uniform(10). %% random value between 1 and 10

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% comparison operators, guards
do_compare(A, B) ->
  do_type_value_compare(A, B),
  not do_value_compare(A, B). %% negation of bool

do_type_value_compare(A, B) ->
  C = A =:= B,
  D = A =/= B,
  E = 2 > 1,
  F = 2 < 1,
  G = 2 >= 1,
  H = 2 =< 1,
  (C and D) xor (E and F and G and H).

do_value_compare(A, B) ->
  C = A == B,
  D = A /= B,
  C or D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% if conditionals
vote_legal() ->
  'Thanks for voting!'.

vote_ban() ->
  'Sorry, you can not vote as of now.'.

vote(Age) ->
  if
    Age < 18 ->
      vote_ban();
    Age =:= 18 ->
      io:format("congrats on first vote~n", []),
      vote_legal();
    Age > 18 ->
      vote_legal()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case conditionals
say_hey(Lang) ->
  case Lang of
    french -> 'Bonjour';
    german -> 'Guten Tag';
    english -> 'Hello';
    hindi -> 'Namashkar'
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% recursion, for looping requirements with better memory management

sum_lst([]) -> 0;
sum_lst([H|T]) -> H + sum_lst(T).

factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N-1). %% non-tail recursion

find_factorial(X) ->
  Y = factorial(X),
  io:fwrite("Factorial: ~p\n", [Y]).

factorial_tr(0, F) -> F;
factorial_tr(N, F) -> factorial_tr(N-1, F*N). %% tail-recursive

find_factorial_tr(X) ->
  Y = factorial_tr(X, 1),
  io:fwrite("Tail-recursive factorial: ~p\n", [Y]).

%% for like mess
ffor(0, _) ->
  io:fwrite("0\n"),
  ok;
ffor(Max, Min) when Max > 0 ->
  io:fwrite("~p ", [Max]),
  ffor(Max-1, Min).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% map

map_stuff() ->
  Mi = #{fname=> "James", lname=> "Bond", uid=> 7},
  io:fwrite("My name is ~p...~s ~s\n",
            [maps:get(lname, Mi), maps:get(fname, Mi), maps:get(lname, Mi)]),
  Mi2 = maps:remove(lname, Mi),
  io:fwrite("~p\n~p\n", [maps:keys(Mi2), maps:values(Mi2)]),
  {ok, Mi3} = maps:find(fname, Mi2),
  Mi4 = maps:put(l_name, "Bond", Mi2),
  io:fwrite("~p\n~p\n", [Mi3, maps:values(Mi4)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% record

-record(consumer, {name="", wallet=0.00, spent=0.00}).

record_stuff() ->
  James = #consumer{name="James Bond", wallet=100.00, spent=101.00},
  Jane = #consumer{name="GI Jane", wallet=1000.00},
  io:fwrite("~s owes $~p to ~s\n",
            [James#consumer.name, James#consumer.spent, Jane#consumer.name]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% higher-order functions, lists' map

times2(A) -> A * 2.

fun_stuff(Name) ->
  Fun_Stuff = fun() -> io:fwrite("GI ~s\n", [Name]) end,
  Fun_Stuff().

hof_stuff() ->
  lists:map(fun times2/1, [1,10,100]),
  fun_stuff("Joe").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% file i/o

rw_file() ->
  overwrite_txt("new file\n"),
  append_txt("new line\n"),
  read_txt().

overwrite_txt(N) ->
  {ok, Fh} = file:open("/tmp/erl-temp", [write]),
  file:write(Fh, N).

append_txt(N) ->
  {ok, Fh} = file:open("/tmp/erl-temp", [append]),
  file:write(Fh, N).

read_txt() ->
  {ok, Fh} = file:open("/tmp/erl-temp", [read]),
  {ok, Words} = file:read(Fh, 1024*1024),
  io:fwrite("~p\n", [Words]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% error handling

error_stuff() ->
  error_stuff(10),
  error_stuff(0),
  read_missing().

error_stuff(N) ->
  try
    R = 100 / N,
    R
  catch
    error:badarith ->
    {err, "Can't Divide By Zero"}
  end.

read_missing() ->
  try
    {ok, Fh} = file:open("/should-be-missing", [read]),
    {ok, Words} = file:read(Fh, 1024*1024),
    io:fwrite("~p\n", [Words])
  catch
    _:_ -> "File read error."
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% spawn concurrent tasks

countdown(Max, Min) when Max >= Min ->
  io:fwrite("~p\n", [Max]),
  countdown(Max-1, Min);
countdown(_,_) ->
  ok.

spawner(Max, Min) ->
  spawn(fun() -> countdown(Max, Min) end).

spawner() ->
  spawner(15, 10),
  spawner(5, 0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
