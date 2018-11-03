
## Chapter 1.4 Robustness

Messenger example from previous chapter is a quick code with many corrections required. Like if a client node goes offline before log-off. Since user isn't delete from user-list, can't logon again. If server goes down between sending message to a client, hanging forever in `await_result`.


### 1.4.1 Time-outs

* let's consider our `ping-pong` program, where `pong` exits on receiving `finished`, could also exit if it does not receive a message from ping with a certain time-out

> * time out `after 25000` is started when `receive` is entered
>
> * time out is cancelled if a match is received
>
> * `after` must be last construct in `receive`

```
(pong@pongnode)2> 'chapter1-4-01':start_pong().
true
Pong received ping
Pong received ping
Pong received ping
```

```
(ping@pingnode)1> 'chapter1-4-01':start_ping(pong@pongnode).
<0.66.0>
Ping received pong
Ping received pong
Ping received pong
ping finished
```


### 1.4.2 Error Handling

* a process executing `exit(normal)` or finishing has a normal exit

* process encountering runtime error exits with error and abnormal exit, with `exit(Reason)`; here Reason could be any erlang term except `normal`

* erlang process can set up links to other erlang process
> * if process calls `link(Other_Pid)` it sets up a bidirectional link between itself and process with `Other_Pid`
>
> * when process terminates, it sends a `signal` to all processes it has links to
>
> * `signal` carries info about pid, it was sent from and the exit reason
>
> * default behavior of a process that receives a normal exit is to ignore a signal
>
> * default behavior in the two other cases (i.e. abnormal exit) above is to:
> > * bypass all messages to receiving process
> >
> > * kill receiving process
> >
> > * propogate the same error signal to the links of the killed process
>
> * this way you can connect all processes in transaction together using links
>
> * if one process dies, all linked in transaction are killed
>
> * often desried to build a process and link at same time, there is a special BIF `spawn_link` for this

* module [chapter1-4-02.erl](./chapter1-4-02.erl) is a `ping-pong` example which uses links to terminate pong

```
(ping@nodeX)4> 'chapter1-4-02':start('ping@nodeX').
Pong got ping
<0.80.0>
ping got pong
Pong got ping
ping got pong
Pong got ping
ping got pong
```

* it's possible to modify process' default behavior to avoid exit on abnormal signal
> * all signals are turned into normal messages like `{'EXIT', FromPID, Reason}` and added to end of receiving process' message queue
>
> * `process_flag(trap_exit, true)` can set this behavior

```
(ping@nodeX)6> 'chapter1-4-02':jingJong('ping@nodeX').
jong...jing...<0.88.0>
jong exits, got {'EXIT',<0.88.0>,normal}
```


### 1.4.3 The Larger Example with Robustness Added

* module [chapter1-4-03.erl](./chapter1-4-03.erl) uses `process_flag` to make crashed clients to logout

> * if server node crashes, all client processes are sent a signal to exit
>
> * also, a timeout of 30seconds of waiting for reply from server makes client terminate

server logs for 2 clients 'bob' and 'eve'

```
(messenger@nodeX)1> 'chapter1-4-03':start_server().
server node: 'messenger@Cr4Sh-0v3rrid3'
true
list is now: [{<10197.81.0>,eve},{<10195.83.0>,bob}]
list is now: [{<10195.83.0>,bob}]
```

client logs for 'bob' which send message twice once when 'eve' is online and when not

```
(c1@nodeY)1> 'chapter1-4-03':logon(bob).
true
logged_on             

(c1@nodeY)2> 'chapter1-4-03':message(eve, "hoy").
ok
sent                  

(c1@nodeY)3> 'chapter1-4-03':message(eve, "hoy").
ok
receiver_not_found
```

client logs for 'eve' which receives a message then crashes before receiving second

```
(c2@nodeZ)1> 'chapter1-4-03':logon(eve).
true
logged_on             
message from @bob: "hoy"

(c2@nodeZ)2> 
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a
```

---
---
