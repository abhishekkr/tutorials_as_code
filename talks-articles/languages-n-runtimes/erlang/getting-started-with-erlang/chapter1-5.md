
## Chapter 1.5 Records and Macros

Large programs written as collection of files with well-defined interfaces between various parts.

### 1.5.1 The Larger Example Divided into Several Files

* messenger example from previous section is divided into following five files
> * [mess_config.hrl](./chapter5-01/mess_config.hrl) header file for configuration data
>
> * [mess_interface.hrl](./chapter5-01/mess_interface.hrl) interface definitions between client and messenger
>
> * [user_interface.erl](./chapter5-01/user_interface.erl) functions for user interface
>
> * [mess_client.erl](./chapter5-01/mess_client.erl) functions for messenger client
>
> * [mess_server.erl](./chapter5-01/mess_server.erl) functions for messenger server

* the message passing interface between shell, client and server is cleaned up and defined using `records`

* `macros` used as well

compiling required code

```
(messenger@nodeX)1> c('mess_client.erl').
{ok,mess_client}
(messenger@nodeX)2> c('mess_server.erl').
{ok,mess_server}
(messenger@nodeX)3> c('user_interface.erl').
{ok,user_interface}
```

starting server and keeping tabs for changing user lists

```
(messenger@nodeX)4> 'mess_server':start_server().
User list = []
true
User list = [{<10172.66.0>,bob}]
User list = [{<10174.66.0>,eve},{<10172.66.0>,bob}]
User list = [{<10174.66.0>,eve},{<10172.66.0>,bob}]
User list = [{<10174.66.0>,eve}]
User list = [{<10174.66.0>,eve}]
User list = []
```

client bob from c1@nodeY and eve from c2@nodeZ

```
(c1@nodeY)1> 'user_interface':logon(bob).
true
logged_on
Message from eve: "hola"

(c2@nodeZ)1> 'user_interface':logon(eve).
true
logged_on

(c2@nodeZ)2> 'user_interface':message(bob, "hola").
ok
sent

(c1@nodeY)4> 'user_interface':logoff().
logoff

(c2@nodeZ)3> 'user_interface':message(bob, "hola").
ok
receiver_not_found
```

---

### 1.5.2 Header Files

* files with extension `.hrl` are header files included by `.erl` files
> * by `include("FileName").` like `-include("some_interface.hrl").`

* these can contain any valid erlang code, but are mostly used for record and macro definitions

---

### 1.5.3 Records

* record is defined as

```
-record(name_of_record, {field_name1, field_name2, .......}).

%% example is equivalent to '{message_to, To_Name, Message}'
%% -record(message_to, {to_name, message}).
```

* record can be created as `#message_to{message="hola", to_name=bob}`
> * it creates `{message_to, bob, "hola"}`

* don't worry about order of assigned values to record fields
> * advantage of using records is placing their definitions in header files, you can define interfaces easy to change
>
> * any field left when creating a record, it gets value of atom `undefined`

* pattern matching is very similar to creating records, example inside a `case` or `receive`

```
#message_to{to_name=ToName, message=Message} ->

%% is same as '{message_to, ToName, Message}'
```

---

### 1.5.4 Macros

* file [mess_config.hrl](./chapter5-01/mess_config.hrl) contains definition of `server_node`

* this file is included in [mess_server.erl](./chapter5-01/mess_server.erl); there every occupance of `?server_node` gets replaced with defined value

* also used when spawning the server process `spawn(?MODULE, server, [])`; this `?MODULE` is a standard macro defined by system

* the compiled `*.beam` gets loaded and linked when executed, from current dir; they can be loaded from different dirrectories by other ways

---
---
