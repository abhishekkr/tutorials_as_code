
### 01. WebSocket - a HandShake

Defines a persistent 2-way communication between webserver and clients. Offer concurrency and high performance for responsive and rich webapps.

#### Life before WebSocket

* Polling
Sync method with periodic requests. Simple but wasteful on time and resources.

* Long Polling
Polling with connection kept alive for longer duration.

* Streaming
Best option for real-time data transmission. Client performs request, server keeps connection open.

* Postback and AJAX
In 2005, that postback flickering was bypassed thanks to AJAX.

---

#### Then Came HTML5

* Markup: Structural elements, Form elements, Attributes
* Graphics: Stylesheets, Canvas, SVG, WebGL
* Multimedia: Audio, Video
* Storage: Cache, Local Storage, WebSQL
* Connectivity: WebMessaging, WebSocket, WebWorkers
* Location: Geolocation

---

#### The WebSocket Protocol

It redefines full-duplex communication. With WebWorkers, take an enormous step in functionality.


* The URL

```
ws://example.com:8000/chat.php
```

> First thing to notice is `ws` prefix. WebSocket over SSL comes off as `wss`.
> RFC-6455


* Who's Using WebSocket

Most well-known paradigm is [Kaazing](http://demo.kazing.com/livefeed), a startup that raised 17mil dollars for real-time communication platform.

> Other businesses include following:
> [Gamooga](http://www.gamooga.com/) - Real-time backend for apps and games
> [GitLive](http://gitlive.com) - Notifications for Github projects
> [Superfeedr](http://Superfeedr.com) - Real-time data pushing
> [Pusher](http://pusher.com) - Scalable Real-Time functionality API for web and mobile apps
> [Smarkets](https://smarkets.com) - Real-time betting
> [IRC Cloud](https://www.irccloud.com/) - Chatting

Resources with great demos
> * [websocket.org](http://www.websocket.org/demos.html)
> * [html5rocks](http://www.html5rocks.com/en/features/connectivity)

---

#### Mobile?

WebSocket API is usable in Native Apps on all major platforms.

---
---
