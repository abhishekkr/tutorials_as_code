
### 02. The WebSocket API

we need to parties to establish it, a server and a client

#### HTML5 Basics

* Markup

Popular new HTML5 tags include `header, article, footer, aside and nav` tags.
Simple example of HTML5 markup code that generates essential elements for our chatting app. Text field to type message, first button will send message, second button will terminate chat, label to display interactions coming from server.

[html5 doctor](http://html5doctor.com/element-index) for more info on HTML5 markup, complete reference for supported HTML5 tags


* Styling

[CSS3](http://www.css3.info) great resource.


* Logic

to handle responsive actions


[app-markup](./chap02-code01.htm)

---

#### API Overview

Translating these characteristics into actions, the WebSocket API allows you to connect to a local or remote server, listen for messages, send data and close connection.

Here is a typical usage of WebSocket workflow

```
Check for WebSocket browser support ->
Create an instance of the WebSocket JS object ->
Connect to the WebSocket server ->
Register for WebSocket events ->
Perform the proper data transmission according to the users' actions ->
Close the connection
```


* Browser Support

JavaScript provides an easy way to find whether a browser can execute WebSocket-specific code
> sample in [app-markup](./chap02-code01.htm)


* The WebSocket Object

Initialize a connection to server, all we need is create a WebSocket JS object as
```
// a public address available for tests, opens a connection to specified server
var socket = new WebSocket("ws://echo.websocket.org");
```


* Events

Four main events on WebSocket API are Open, Message, Close and Error. Can be handled be implementing `onopen, onmessage, onclose and onerror` functions, or by using `addEventListener` method.
> * onopen
> raised right after connection got established and ready to transmit data
> * onmessage
> is client's ear to server
> * onclose
> if happens due to error provides with cause and code
> * onerror
> raised on any unexpected behaviour


* Actions

Raised when something happens, explicit calls are made when needed. Protocol supports 2 main actions: send & close.
> * send()
> send method allows to transfer variety of data to websocket server, shall check for `readyState` of socket
> * close
> stands as goodbye handshake()


* Properties

WebSocket exposes some properties. Main are
> url : returns url of WebSocket
> protocol : returns protocol used
> readyState : returns state of connection as WebSocket.{OPEN,CLOSED,CONNECTING,CLOSING}
> bufferedAmount : returns total byte count queued when `send()` was called
> binaryType : returns binary data format received when `onmessage` event got raised


* The Complete Example

[app-markup](./chap02-code01.htm) has all sample code in working fashion


* What about the server?

this covered websocket client primarily, server later

---
---
