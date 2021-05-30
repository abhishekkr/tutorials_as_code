
## Chapter.6 Generators and Relationships

> contexts continue being business logic API accessed by controllers

### Using Generators

#### Adding Video and Annotations

> Videologue will allow users chosing a Video, attach comments to it (annotate) in real-time.. users to be able to play back these videos with comments over time.

* will be using Generators to build skeletal code alongwith migration, context, controllers and templates

#### Generating Web Interfaces

* Mix task `phx.gen.html` creates simple HTTP CRUD scaffold with HTML pages; `phx.gen.json` does same for REST-based API using JSON

* while organizing code, think of Contexts first.. where does Video resource reside; in future Videologue might evolve to different media channel annotations

> a `Multimedia` context gives good place to group Video functionality
>
> * few fields for Video would be associated `user`, `creation time`, `URI`, `title`, `description`

* generate Multimedia resource as

```
mix phx.gen.html Multimedia Video videos user_id:references:users url:string title:string description:text
```

> this generates 
> controller `lib/videologue_web/controllers/video_controller.ex`,
> view `lib/videologue_web/views/video_view.ex`,
> templates `lib/videologue_web/templates/video/{edit.html.eex,form.html.eex,index.html.eex,new.html.eex,show.html.eex}`,
> context `lib/videologue/multimedia.ex` with schema `lib/videologue/multimedia/video.ex`,
> db migration `priv/repo/migrations/20210529175006_create_videos.exs`,
> alongwith tests boilerplate for controller & context
>
> * expects to explicitly add `resources "/videos", VideoController` and run Ecto migrations

* sometimes Generators are too much, pays to be explicit

* Phoenix uses Singular forms consistently across schema, controller & views in most cases

* move `UserController.authenticate` to [VideologueWeb.Auth](lib/videologue_web/controllers/auth.ex)'s `authenticate_user` for central reuse at User and Multimedia checking sessions

> * add `import VideologueWeb.Auth, only: [authenticate_user: 2]` to `controller` & `route` macros in [videologue\_web.ex](videologue/lib/videologue_web.ex)
>
> * and update the Plug at [UserController](lib/videologue_web/controllers/user_controller.ex) for new name i.e. `authenticate_user`
>
> * create `/my` scope for managing Videos in [router.ex](lib/videologue_web/router.ex) and add `:authenticate_user` to `pipe_through` list alongwith `:browser` to be used for all resources under it
>
> * add `VideoController` resource to `/my` scope
>
> * now visit [/my/videos](http://localhost:4000/my/videos) to add a video

#### Examining Generated Context, Controller, View

* updated Show/Index templates to have clickable URL rendered; and `validate_length` for minimum `description` length

#### Generated Migrations

* are for all fields added with up `mix ecto.migrate` & down `mix ecto.rollback`

* it takes care of adding Foreign Key


### Building Relationships

* default generated Schema sets up `user_id` of type `:id`; migration sets it up as Foreign Key

> * replace `field :user_id, :id` by `belongs_to :user, Videologue.Accounts.User` allowing Ecto to build right associations
>
> * it's not cast-able or required; the earlier created Video wouldn't have this field yet
>
> * creating a new video & accessing `video.user` will give `#Ecto.Association.NotLoaded<association :user is not loaded>`

* Ecto associations are explicit, to fetch need to ask to avoid cascade fetch not needed

> * `video = Videologue.Repo.preload(video, :user)` makes it load, value yet is `nil`

* `Repo.preload` accepts one name or collection of association names

* let's update the existing video for `user_id` via IEx

```
iex(1)> video = Videologue.Repo.get(Videologue.Multimedia.Video, 1)
%Videologue.Multimedia.Video{
  __meta__: #Ecto.Schema.Metadata<:loaded, "videos">,
  description: "With the Phoenix v1.5 release, learn how easy LiveView makes it to build interactive, real-time applications.",
  id: 1,
...
  user_id: nil
}

iex(2)> video = Videologue.Repo.preload(video, :user)

iex(3)> video.user
nil

iex(4)> user = Videologue.Repo.get(Videologue.Accounts.User, 1)  

iex(7)> video = video |> Ecto.Changeset.change() |> Ecto.Changeset.put_assoc(:user, user) |> Videologue.Repo.update!(changeset)

iex(8)> video.user                                
%Videologue.Accounts.User{
  __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
  id: 1,
...
  username: "sherlock"
}

iex(9)> videoo = Videologue.Repo.get(Videologue.Multimedia.Video, 1)                         
%Videologue.Multimedia.Video{
  __meta__: #Ecto.Schema.Metadata<:loaded, "videos">,
  description: "With the Phoenix v1.5 release, learn how easy LiveView makes it to build interactive, real-time applications.",
  id: 1,
...
  user_id: 1 
}

iex(10)> videoo.user
#Ecto.Association.NotLoaded<association :user is not loaded>

iex(11)> videoo = Videologue.Repo.preload(videoo, :user)                                      

iex(12)> videoo.user                                    
%Videologue.Accounts.User{
  __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
  id: 1,
...
  username: "sherlock"
}
```

> generally avoid cyclic dependencies, so one-way relationship from videos to user


### Managing Related Data

> * need to grab current-user `conn.assigns.current_user` and scope operations to them by passing it in `Videologue.Multimedia.create_video/2` (updated from `create_video/1`)

```
  def create(conn, %{"video" => video_params}) do
    case Multimedia.create_video(conn.assigns.current_user, video_params) do
      {:ok, video} ->
        conn
...
```

> * in `create_video/2` utilize `Ecto.Changeset.put_assoc/2` over changeset to associate User to Video just before `Repo.insert/1`

```
  def create_video(%Accounts.User{} = user, attrs \\ %{}) do
    %Video{}
    |> Video.changeset(attrs)
    |> Ecto.Changeset.put_assoc(:user, user)
    |> Repo.insert()
  end
```

> * similarly update `new`, `edit` & `update` function calls

* BUT here we'll use `current_user` nearly in all calls; let's add `conn.assigns.current_user` to be passed to all Controller functions as an argument by default with a custom `action` function in `VideoController`

> now we have `current_user` as 3rd param in all functions which can then be used with `list_videos` to only show current user videos; `change_video` to edit/update

* pipe `user_videos_query/2` to append FROM checking list/get/delete is only made possible on Current User's already owned content

> `user_videos_query/2` ensuring `get_user_video!` to work only for current-user in-turn ensures `edit`, `update` & `delete` controller task only work for current-user
>
> currently `get_user_video!` is being used which exposes bunch of errors, as discussed earlier these shall be masked alongwith all other 4XX & 5XX


### In-context Relationships

* defining relation within same context like adding Categories to Videos; don't need to create Controller with View/Template as not often added

* all Multimedia resources will have Categories; so schema be `Multimedia.Category`

#### Schema and Context Generators

* `mix phx.gen.html Multimedia Category categories name:string` will generate controller, view, template, context, schema & migration

* `mix phx.gen.context Multimedia Category categories name:string` makes context, schema & migration (for pre-existing context it will be injected)

* `mix phx.gen.schema Multimedia.Category categories name:string` makes a schema with migration

* `mix ecto.gen.migration create_categories` generating new empty migration

#### Generating Category Migrations

* `mix phx.gen.schema Multimedia Category categories name:string` makes a schema with migration

* in Video schema add `belongs_to :category, Videologue.Multimedia.Category` and cast `:category_id` in changeset

* `mix ecto.gen.migration add_category_id_to_video` and add following to generated file and the `mix ecto.migrate`

```
  alter table(:videos) do
    add :category_id, references(:categories)
  end
```

> now we have Ecto level assoc placed and Foreign Key constraints at DB

### Wrapping Up

> [Chapter-07 Ecto Queries and Constraints](./chapter-07.md)

---
