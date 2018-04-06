## Phoenix Framework

[Phoenix](https://phoenixframework.org)
[Elixir](https://elixir-lang.org)

by @josevalim

> Whatsapp : 2mil conn on single node [](http://blog.whatsapp.com/index.php/2012/1-million-is-so-2011); so Erlang

Phoenix in elixir, highly performant

Crashes are isolated. Data is isolated (GC is per process, no global pause).

Got channels directly mapped to client-side websockets. Suited for highly talkative client-server model.

> * Ecto: DB wrapper and query lang
> * Form builders
> * Static build tools with ES6 as default
> * Pretty error pages
> * First class test tools
> * Packages via hex.pm

#### Applications context

Different clients connect initiate different processes bringing in TCP acceptor into play for each connection.
A supervisor to keep check on TCP acceptor service and pub-sub service availability.

'Applications' package and run code. They can be started and stopped as a unit.
They provide unified configuration (provided from language level).
'Applications' have a supervison tree where it holds processes and state.

You can get an 'elixir' console inside an Application.
So you can start 
```
iex(x)> :observer.start
```
to get system details. Helps you introspect.
Gives visibility into application state and helps you decide if a process tree need to break out.

#### from FAQ

* ErlangVM's hot swap of a module is possible given the developer writing module has taken care of it.
> For reference watch talk on 'Poker Game' via Elixir in ElixirConf-2014

* wait for Phoenix to be 1.0 (possibly Aug-2015)

---
