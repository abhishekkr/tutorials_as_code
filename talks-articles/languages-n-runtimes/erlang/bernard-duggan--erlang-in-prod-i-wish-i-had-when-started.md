
## Erlang in Production: "I wish I'd known that when I started"

> by Bernard Duggan at LinuxConfAU 2012


### Overview

* Dialyzer should be mandatory

* The VM can crash like any other unix process

* Message Queues "just work", till they don't

* OTP is invaluable

* Integration as UNIX-style service is lacking

* Hot code loading is... interesting

* System monitoring is vital

---

### Dialyzer

* brings static type-safety to a dynamically typed language

* a static analysis tool that identifies issues like definite type errors, dead code, unreachable code, unnecessary tests

---

### VM Crashing

#### Out Of Memory errors

* due to non tail-recursive loops

```
%% good
good_loop() ->
  do_something(),
  something_more(),
  good_loop(). % Tail-call

%% bad
bad_loop() ->
  do_something(),
  something_more(),
  bad_loop(),
  ok. % damn

%% also bad
also_bad_loop() ->
  V = do_something(),
  case V of
    done -> 1;
    continue -> 1 + also_bad_loop() % also need to maintain stack through recursions
  end.

%% again bad
again_bad(X) ->
  try
    case Fn(X) of
      continue -> again_bad(Y);
      done -> ok
    end
  catch
    _ -> boom()
  end. % try-catch need to maintain stack as well

%% better
better_try(X) ->
  try Fn(X) of % exceptions thrown here are not caught; so stack is not kept
    Y -> better_try(Y);
    _ -> ok
  catch
    _ -> boom()
  end.
```

* due to queue overflow

> * message queues are powerful but can get in trouble with overload or selective `receive`
>
> * selective receive allow interesting flows and thus found in places like `mnesia:transaction/1`; can take hours or days to cause problems so need to be monitored
>
> * selective receive is somewhat mitigated as R14 with reference optimization, but not all cases like of mnesia

```
%% simple overload, this called by several processes
%% no internally enforced limit on msg queues
log_info(Msg) ->
  logger ! {info, Msg}.
logger() ->
  receive
    {info, Msg} -> format_and_write_info(Msg);
    _ -> ok
  end,
  logger().

%% selective receive
%% have to look lineraly through entire message queues
receiver() ->
  receive
    one_kind_message -> do_something()
  end,
  receive
    other_kind_message -> do_other_thing()
  end,
  receiver().

%% new reference optimization
R = make_ref(),
server ! {R, MyRequest},
receive
  {R, Resp} -> process_response(Resp)
end
```

* Linked-in drivers, NIFs (Native Implemented Functions)

> implemented C extensions can get wild

---

### OTP (Open Telephony Platform)

> nothing to do directly with telephony except quality required

* architecture for writing robust long running applications

* forces to consider process interaction, failure modes, crash behavior and more
> * get a Supervision Tree
>
> * forces to build process tree of supervisors and processes
>
> * can let a process crash, make supervisor restart a process or group of process in case any of it crashes

* possibly overkill for small projects, suggested for anything else

* solves problems a better way

```
%% making a call to another process, first try
server_proc ! {request, ReqData},
receive
  {response, RespData} -> RespData
end.

%OTP% but to be sure of correct response
Ref = make_ref(),
server_proc ! {request, Ref, ReqData},
receive
  {response, Ref, RespData} -> RespData
end.

%OTP% what if the server process doesn't exist
case whereis(server_proc) of
  undefined -> {error, noproc};
  Pid ->
    Ref = make_ref(),
    server_proc ! {request, Ref, ReqData},
    receive
      {response, Ref, RespData} -> {ok, RespData}
    end,
end.

%OTP% what if server process died after call
case whereis(server_proc) of
  undefined -> {error, noproc};
  Pid ->
    Ref = make_ref(),
    server_proc ! {request, Ref, ReqData},
    receive
      {response, Ref, RespData} -> {ok, RespData};
      after 5000 -> {error, timeout}                % 5sec timeout
    end,
end.

%OTP% nice not to wait 5s if process crashed
case whereis(server_proc) of
  undefined -> {error, noproc};
  Pid ->
    MRef = erlang:monitor(process, server_proc),
    Ref = make_ref(),
    server_proc ! {request, Ref, ReqData},
    receive
      {response, Ref, RespData} ->
        erlang:demonitor(MRef),
        {ok, RespData};
      {'DOWN', MRef, _, _} -> {error, no_proc};
      after 5000 ->
        erlang:demonitor(MRef),
        {error, timeout}                % 5sec timeout
    end,
end.
```

* if remote (C/Java) node doesn't support `erlang:monitor`
> * 12+ lines of code for simple call is anyway too much
>
> * any client can call `gen_server:call(server_proc, {request, ReqData})` for appropriate response

* get [EventHandlers](http://erlang.org/doc/design_principles/events.html) (subscribe-notify)
> * `EventManager` can have zero-to-many event handlers and can receive events
>
> * can be programmed using `gen_event` behavior

* [FSMs](http://erlang.org/documentation/doc-4.8.2/doc/design_principles/fsm.html), many applications can be designed as Finite State Machines and be programmed using `gen_fsm` behavior

---

### As a Unix Service

* has an embedded heritage, turn on device and walk away

* usual startup `erl -noshell -detatched -boot app.boot` always returns 0, `-detatched` means no console output

* can use [some tricks](https://medium.com/@max.lapshin/how-to-start-and-stop-erlang-daemon-3fd988777ab3) to manage it elegantly

* no `.pid` file, naively can write from erlang (but what if code fails)

* `heart` is a built-in VM monitoring program, can make shutdown of broken VMs difficult; `kill -stop` helps

* log rotation is unusual, no way to handle `SIGHUP`

* this makes distro package manage standard upgrade difficult

* [erld](https://github.com/ShoreTel-Inc/erld) working on principle of `GNU screen`
> * wraps erl and holds its terminal; programatically detaches console
>
> * logs console output; intercepts SIGHUP; returns useful error; manage crash/restarts

---

### Hot Code Loading

* can get insane uptimes by applying patches to running code without dropping network connections

* doing it correctly is a bit hard; testing is harder

---

### System Monitoring

* lot of ways to monitor different parts; but need to use them

#### Key monitoring points

* number of process to avoid process leaks `length(erlang:processes())`

* queue length for busy processes `erlang:process_info(Pid, message_queue_len)`

* total memory usage `erlang:memory/0,1`

---
---
