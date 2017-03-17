
### 03. Configuring the Server

#### Why do I need a WebSocket server?

Following diagram illustrates communication process between a WebSocket server and clients

```
  [Server]                                         [Client]
 create server on svr:8181
   |,
 start running

 initial handshake, establish connection <---- request connection

 handle incoming message <-------------------- send message
```

---

#### Setting up the Server

* Selecting the technology that suits you
> * C/C++
> [tufao](https://github.com/vinipsmaker/tufao), [wslay](http://wslay.sourceforge.net), [libwebsockets](http://libwebsockets.org/trac), [mongoose](https://code.google.com/p/mongoose)
>
> * Java
> Apache Tomcat, JBoss, Glassfish, Jetty, jWebSocket, [atmosphere](https://github.com/Atmosphere/atmosphere), [Play](http://www.playframework.com), [Migratory Data](http://migratorydata.com), [bristleback](http://bristleback.pl)
>
> * .Net
> IIS8, [Fleck](https://github.com/statianzo/Fleck), [SuperWebSocket](http://superwebsocket.codeplex.com/)
>
> * PHP
> [php-websocket](https://github.com/nicokaiser/php-websocket), [Rachet](http://socketo.me), [Hoar](https://github.com/hoaproject/Websocket)
>
> * Python
> [tornado](http://www.tornadoweb.org/en/stable/), [Pywebsocket](http://code.google.com/p/pywebsocket/), [Autobahn](http://autobahn.ws), [txWS](https://github.com/MostAwesomeDude/txWS), [WebSocket for Python](https://github.com/Lawouach/WebSocket-for-Python)
>
> * Ruby
> [em-websocket](https://github.com/igrigorik/em-websocket), [socky](https://github.com/socky/socky-server-ruby)
>
> * JavaScript
> [socketIO](http://socket.io), [websocket-node](https://github.com/Worlize/WebSocket-Node), [Node WS Server](https://github.com/miksago/node-websocket-server)

* Setting up the development environment

---

#### Connecting to the WebServer

Works similar to Clients, responds to events and performs actions when necessary. It handles `OnOpen, OnClose and OnMessage` events.

---

#### Other Methods

Depending on which WebSocket server implementation you use, might be additional events or methods.

---
---
