
## Chapter.10 Using Channels

> here is catered high interactive problem solved well by Phoenix
>
> since Elixir can scale to millions of simultaneous processes managing concurrent connections; don't have to Request/Response, client can connect a channel and send/receive messages

### The Channel

* Phoenix Channel converses over a `topic` sending/receiving `events`(messages) and keeping state(in socket `struct`)

* More than a user might want same topic at a time; each user's conversation has an isolated process

* Request/Response are stateless here, these long running conversation can be stateful.. so don't need to keep track using cookies/DB/similar

* This only works due to true isolation & concurrency granted by base; here one crashing user wouldn't impact another

* Channels application gotta worry of 3 things (on both client & server): managing connections; sending messages; receiving messages

> users will add Annotations in Real-time; Videologue will play back video annotations for a user
>
> * first on client side, add ES6 client code to do the aforementioned 3 things; then on server side, do the same
>
> * then utilize Channel Presence, allowing users to know who is logged in


### Phoenix Clients with ES6

* here Channel topics at Client would be Videos; client-side [Video object](videologue/assets/js/video.js) construct will directly connect with Phoenix

* update [app.js](videologue/assets/js/app.js) to use `Video` js object instead of `Player` object, passing it `socket` as well

> * this works but gives `Unable to join {reason: "unmatched topic"}` Browser console error>
> * with `vidChannel` initialized in `Video` client tries joining video channel


### Preparing our Server for the Channel

* for each request a new `Plug.Conn`; channel flow is different as after connection is made the socket will be transformed through life of connection

* first decide whether to allow connection; next create initial socket including any custom application setup

* `endpoint.ex` already includes `socket` mount point & `websocket`, `longpoll` configuration

* [user\_socket.ex](videologue/lib/videologue_web/channels/user_socket.ex) gets mounted at Endpoint by default with `connect/3` & `id/1` defined

> * `id/1` help identify socket based on state stored in socket itself; returning `nil` lets everyone in anonymously
>
> * `connect/3` decides whether to make connection, receiving conn params, socket & advanced config map

* `UserSocket` uses single connection to server handling all channels; Phoenix handle routing right message to right channel


### Creating the Channel

* our `topic` has identifier of `videos:video_id`; we want all users to get Annotations created on a Video regardles of who posted it

#### Joining a Channel

* need a [VideoChannel](videologue/lib/videologue_web/channels/video_channel.ex) for our application, to be included as a channel def in `UserSocket` as

```
  channel "videos:*", VideologueWeb.VideoChannel
```

#### Building the Channel Module

* creating `VideoChannel`, add `join/3` to extract `video_id` from passed `videos:video_id` topic-subtopic in connection request and assign that on socket map

* add `vidChannel.join()..` with `receive()` callbacks for "ok" & "error"

> current state gives following in browser console logs

```
transport: connected to ws://localhost:4000/socket/websocket?token=undefined&vsn=2.0.0
player.js?7e1b:26 ready {target: ej, data: null}
socket.js?1554:6 push: phoenix heartbeat (undefined, 1) {}
socket.js?1554:6 receive: ok phoenix phx_reply (1) {response: {…}, status: "ok"}
```


### Sending and Receiving Events

* channel module receive events via callbacks

> * `handle_in` receives direct channel events
>
> * `handle_out` intercepts broadcast events
>
> * `handle_info` receives OTP messages

#### Taking Our Channels for a Trial Run

* client does get a Phoenix heartbeat; lets send custom `:ping` to channel to test our flow

* `handle_info` callback is added to `VideoChannel` to keep a counter pushed to `:ping`

* add `vidChannel.on(event, callback)` API to handle ping and console log the counter in it; via `video.js`

> Controllers process a `request`, Channels hold a `conversation`

#### Annotating Videos

* add `vidChannel.on(event, callback)` API to handle `new_annotation` in `video.js`; it to call `renderAnnotation` which adds the user & annotation detail to `msgContainer` with safety escape from XSS

#### Adding Annotations on the Server

* `handle_in/3` manages all incoming requests to a channel

* `broadcast!/3` sends event to all users on current topic; using Phoenix Pub/Sub in background

> `broadcast!` allows sending `:ok` and `:error` as reply or `:noreply` to skip sending

* add `handle_in("new_annotation", params, socket)` to `VideoChannel` to broadcast any annotation posted

> don't send all payloads as that will allow users to send arbitrary data across a topic


### Socket Authentication

* for channel communication, token authentication works better.. so using `token` at `websockets` will not make `cookies` available

> generate a token for authenticated user and pass it to socket at frontend

* add `window.userToken="<%= assigns[:user_token] %>"` JS snippet to [app.html.eex](videologue/lib/videologue_web/templates/layout/app.html.eex)

* add `put_user_token/2` with `Phoenix.Token.sign` to [Auth](videologue/lib/videologue_web/controllers/auth.ex) controller which gets used in `call/2` & `login/2`

> our `socket.js` already passes along `window.userToken` as Socket params

* add `Phoenix.Token.verify` at `UserSocket` to check upon token in socket


### Persisting Annotations

> now to persist currently in-memory annotations, so they can be replayed

```
# mix phx.gen.schema Multimedia.Annotation annotations body:text at:integer user_id:references:users video_id:references:videos

* creating lib/videologue/multimedia/annotation.ex
* creating priv/repo/migrations/20210621113210_create_annotations.exs

# mix ecto.migrate
```

* add `has_many :annotations` directive to [videos](videologue/lib/videologue/multimedia/video.ex) schema

* replace `user_id` & `video_id` in [Videologue.Multimedia.Annotations](videologue/lib/videologue/multimedia/annotation.ex) with `belongs_to` directives

* add `annotate_video/3` (which Repo inserts) & `list_annotations/1` (which fetchs from DB) to [Videologue.Multimedia](videologue/lib/videologue/multimedia.ex)

* update `handle_in/3` to pull `user` using `socket.assigns.user_id` made to put user detail in `broadcast!`

* add a `render` for `user.json` to [UserView](videologue/lib/videologue_web/views/user_view.ex) for id & username

> the annotations disappear on refresh

* update `VideoChannel's join` callback to pass down list of annotations by using Phoenix's 3-tuple `join` signature as `{:ok, join_response, socket}` to join channel and send a join response using `AnnotationView`

* update `video.js` for `vidChannel.join()...` under `"ok"` to call `renderAnnotation` for each annotation received

```
    vidChannel.join()
      .receive("ok", ( {annotations} ) => {
        console.log("joined video channel", annotations)
        annotations.forEach( ann => this.renderAnnotation(msgContainer, ann) )
      })
      .receive("error", reason => console.log("join failed", reason))
```

> this ensures annotations persist & gets loaded on fresh page load
>
> now we will update `video.js` to schedule annotations to appear synced with the video progress they were posted `at`

* update `video.js` with `vidChannel.join()...` calling `scheduleMessages()` instead which runs every second & renders anything for which time has come

> now use `data-seek` in annotations added to `msgContainer` as click-able entity to skip to the time in video for that comment to be made

* add a `click` event listener to `msgContainer` utilizing `Player.seekTo`


### Handling Disconnects

> currently once client connect to server and post few annotations; if client rejoins say after a server crash it duplicate posts all annotations in `msgContainer`

* track a `last_seen_id` on client bumped on every new annotation; which can be checked on rejoin

```
//before adding last_seen_id
    let vidChannel = socket.channel("videos:" + videoId)

// can send last_seen_id as param to VideoChannel & make it filter sent annotations
    let vidChannel = socket.channel("videos:" + videoId, () => {
        return {last_seen_id: lastSeenId}
      })
```

* update `vidChannel.join()..` to set `lastSeenId` to max response's annotation id available; so Channel knows to filter based on that and send only newer annotations on reconnect

* update `Multimedia.list_annotations` with `where: a.id > ^since_id` by assing a param `since_id` with default value 0, which could get required `since_id` from `VideoChannel.join/3`


### Tracking Presence on a Channel

* tracking which users are watching a video, cleaning up after they disconnect.. `Channel Presence`

* generate presence files `mix phx.gen.presence` creating [presence.ex](videologue/lib/videologue_web/channels/presence.ex)

> we also need to add it to Supervision tree as `VideologueWeb.Presence`
>
> * so `send(self(), :after_join)` in `VideoChannel.join()` to allow itself let know of joining successfully
>
> * `handle_info(:after_join, socket)` gets invoked on successful join & will process the message above to call `VideologueWeb.Presence.track`
>
> * hardcoding device as `browser` as planning to only target those

* add `user-list` element in `watch/show.html.eex`; using `video.js` append user elements to it

* add `import {Presence} from "phoenix"` on top of `video.js` and in `onReady` add

```
    let userList = document.getElementById("user-list")

    let presence = new Presence(vidChannel)
    presence.onSync(() => {
        userList.innerHTML = presence.list((id, {metas: [first, ...rest]}) => {
              let count = rest.length + 1
              return `<span>${id} (${count})</span>`
            }).join(", ")
      })
```

#### Decorating Entries with Application Data

> can decorate `presence` information by defining `fetch/2` and updating the `metas` returned

* add `list_users_by_ids/1` to `Videologue.Accounts`

* add `fetch/1` to `VideologueWeb.Presence`


### Wrapping Up

> [Chapter-11 Observer and Umbrellas](./chapter-11.md)

---
