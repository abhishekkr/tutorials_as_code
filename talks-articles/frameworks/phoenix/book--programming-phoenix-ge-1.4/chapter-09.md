
# Part.II Writing Interactive and Maintainable Applications

> channels for concurrency; service layers with OTP API


## Chapter.9 Watching Videos

### Watching Videos

> * Currently all Video scopes are restricted, no Public URL to share to watch Videos

* add [WatchController](videologue/lib/videologue_web/controllers/watch_controller.ex) for watching user videos added by any

* create a [view module](videologue/lib/videologue_web/views/watch_view.ex) & [show template](videologue/lib/videologue_web/templates/watch/) for Watch

> * View to provide a `player_id/1` function providing Video Id doing Regex parsing for Youtube URLs
>
> * template is mostly markup, providing Video content

* add Route for it `get "/watch/:id", WatchController, :show`

* add a Public Link to Videos listing page alongwith show; and on Show.. also make Listing page a bit leaner


### Adding JavaScript

* `webpack` gets used to build/transform/minify JS/CSS alongwith other assets as images

* anything that doesn't need to be transformed goes into `assets/static`; they plainly get copied to `priv/static` & served using `Plug.Static` in Endpoint

* css goes at `assets/css`, js into `assets/js`; 3rd party libs as jQuery goes in `assets/vendor`

* Phoenix makes Webpack use ES6 to provide required `import` statements

* each file is loaded in a function, not auto executed by browsers unless explicitly imported in [app.js](videologue/assets/js/app.js)

* although js under `assets/vendor` gets auto concatenated to `app.js` & executed when page loads

> `app.js` acts like a Manifest

* command `webpack --watch` just compiles assets into static files & copies to `priv/static`; the switch `--watch` makes it monitor file changes

> with `webpack --mode production` to do almost everything required for prod assets

* write js for [Player](videologue/assets/js/player.js) object to handle video player details

> * wires up `window.onYoutubeIframeAPIReady` callback; we inject a YT iframe tag which triggers our event when player is ready
>
> * implement `onIframeReady` function to create player with YT iframe API
>
> * this abstraction builds an API for video players; will let add other video supports over time

* we'll need to import Player in [app.js](./videologue/assets/js/app.js) and call `Player.init` with Video element Id if available

> this completes a Publicly Watchable link to Video with embedded iframe for it, space for annotations to show and comment to be posted


### Creating Slugs

* making Videos have title friendly URL as `/<id>-<tacked-title>`, add a Slug

* first `mix ecto.gen.migration add_slug_to_videos` generating a migration file, add `alter table(:videos) ..` to `add :slug, :string`

> now `mix ecto.migrate` to apply it

* Ecto allows to have separate change policy for each field; to filter/cast/validate/compose

> add `field :slug, :string` to [Video](videologue/lib/videologue/multimedia/video.ex) and pipe `slugify_title/1` to put `slug` into non-erroneous changeset

#### Extending Phoenix with Protocols

> now to fix Routes and Routing for use of Slug

* changing route call at `video#{index,show}` to use `slug` based route tokens `to: Routes.watch_path(@conn, :show, "#{ video.id }-#{ video.slug }")`

> but this Routes way is not DRY as we had to concat at index, show

* customize how Phoenix generates URLs for videos; implementing protocol `Phoenix.Param` for our custom Video type at [param.ex](videologue/lib/videologue_web/param.ex)

> * the above Param implementation makes Video `show` route change which when passed to `watch_path` & `video_path` gets similar slug just different base-path
>
> since this implementation could reside out of Video module, provides a cleaner polymorphism

```
iex(1)> v = %Videologue.Multimedia.Video{id: 1, slug: "oye"}

iex(2)> VideologueWeb.Router.Helpers.watch_path %URI{},  :show, v
"/watch/1-oye"

iex(3)> VideologueWeb.Router.Helpers.video_path %URI{},  :show, v
"/my/videos/1-oye"

iex(4)> VideologueWeb.Endpoint.struct_url() |> VideologueWeb.Router.Helpers.watch_url(:show, v)        
"http://localhost:4000/watch/1-oye"
```

#### Extending Schemas with Ecto Types

> the Route gets generated but not identified, as `Repo.get!` happens by `id` for that

* extend behavior to `id` fields with custom type [Permalink](videologue/lib/videologue/multimedia/permalink.ex) as new behavior (implementation of `Ecto.Type`)

* Ecto.Type behaviour expects 4 functions; `type` to return underlying type; `cast` invoked when values in queries are interpolated or changesets; `dump` when persisted to db; `load` when read from db

> * `dump` & `load` handles struct-to-db conversion
>
> * `cast` generally deals with user input and hence sanitization
>
> our works well for any string starting with a positive integer as ID should

```
iex(1)> Videologue.Multimedia.Permalink.cast "1"
{:ok, 1}
iex(2)> Videologue.Multimedia.Permalink.cast "1-hello"
{:ok, 1}
iex(3)> Videologue.Multimedia.Permalink.cast "1-007hello"
{:ok, 1}
```

* add following above `schema` def in `Videologue.Multimedia.Video` to make it use custom type as primary-key id

```
@primary_key {:id, Videologue.Multimedia.Permalink, autogenerate: true}
```

> the Show URLs will work now with human friendly URLs


### Wrapping Up

> [Chapter-10 Using Channels](./chapter-10.md)

---
