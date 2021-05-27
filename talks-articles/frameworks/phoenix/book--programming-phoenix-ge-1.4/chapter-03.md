
## Chapter.3 Controllers

> sample Project would be `videologue`; allowing live chat over elsewhere hosted videos embbeded at it

### Understanding Controllers

* we are going to handle users; for a controller that handles users

```
connection |> endpoint() |> router() |> browser_pipeline() |> UserController.action()

# breaking down last part further

..connection |> UserController.index() |> UserView.render("index.html")
```

> * request enters via `lib/videologue_web/endpoint.ex`; then to `...videologue_web/router.ex`; then calling `UserController` action
>
> * assuming request invokes `index` function the View and Template constructs need be prepared

#### The Context

* Phoenix Context groups function with common buziness logic purpose; like CRUD for Users can be in a single module

* Controller exists to work with Context and translate their result into something for end-user

* Context are unaware of Controllers

* Business Logic Changes would just restrict to Context; Unit test business logic via Context API; integration tests focused on Contoller

#### Creating the Project

```
mix phx.new videologue

mix ecto.create

mix phx.server
```

#### A Simple Home Page

* update [index.html.eex](videologue/lib/videologue_web/templates/page/index.html.eex) to a simple Welcome as

```
<section class="phx-hero">
  <h1><%= gettext "Welcome to %{name}!", name: "Video.logue" %></h1>
  <p>Start the videologue..</p>
</section>
```

#### Working with Contexts

> shall act like Bounded Contexts from DDD

* starting with hard-coded data for short term allowing build rapidly and test the app; later replace with full db backed Ecto repo

* create `Accounts` to group user concerns together

> start with file [user.ex](videologue/lib/videologue/accounts/user.ex) with basic struct

```
defmodule Videologue.Accounts.User do
  defstruct [:id, :name, :username]
end
```

#### Elixir Structs

* Elixir Structs are special Maps with `__struct__` key

* add `list_users/0`, `get_user/1`, `get_user_by/1` functions to interact with User struct at [accounts.ex](videologue/lib/videologue/accounts.ex) initiationg Accounts Context

> this would let you do things like

```
iex(1)> Videologue.Accounts.list_users
[
  %Videologue.Accounts.User{id: "1", name: "Alice", username: "Alice InChains"},
  %Videologue.Accounts.User{id: "2", name: "Bob", username: "Bob Cat"},
  %Videologue.Accounts.User{id: "3", name: "Chad", username: "Chad Wick"}
]
iex(2)> Videologue.Accounts.get_user("2")
%Videologue.Accounts.User{id: "2", name: "Bob", username: "Bob Cat"}
iex(3)> Videologue.Accounts.get_user("@")
nil
iex(4)> Videologue.Accounts.get_user_by(name: "Alice")
%Videologue.Accounts.User{id: "1", name: "Alice", username: "Alice InChains"}
```

### Building a Controller

* can create all routes by `resources` macro; but that later

* add `UserController.index` & `UserController.show` routes

> `:show`, `:index`, `:new`, `:create`, `:edit`, `:update`, `:delete` actions are frequently/conventionally used

* add `VideologueWeb.UserController.index/2` at [user\_controller.ex](videologue/lib/videologue_web/controllers/user_controller.ex)

```
defmodule VideologueWeb.UserController do
  use VideologueWeb, :controller

  alias Videologue.Accounts

  def index(conn, _params) do
    users = Accounts.list_users()
    render(conn, "index.html", users: users)
  end
...
```

### Coding Views

* `view` is a module containing rendering functions to HTML/JSON from data

> add [user\_view.ex](videologue/lib/videologue_web/views/user_view.ex) with `VideologueWeb.UserView.first_name/1` returning first-name for a user

* `template` is a function on module; compiled from a file containing raw markup & embedded elixir

> add [index.html.eex](videologue/lib/videologue_web/templates/user/index.html.eex) with `VideologueWeb.UserView.first_name/1` returning first-name for a user
>
> * template here shows multiline split looping with value chaining in html; alongwith `link ...` addition using existing Route function

* templates are functions; Phoenix builds them using linked lists instead of traditional string concat it doesn't have to make giant string copies

> [this blog](https://www.evanmiller.org/elixir-ram-and-the-template-of-doom.html) explains it well
>
> uses I/O List data structure to leverage `writev` to minimize data copies when writing for a limited element list to use no-Copy magic


### Using Helpers

* are simple Elixir functions like `link` before

* helpers are available from `use VideologueWeb, :view` which at `lib/videologue_web.ex#view`

> `view` uses `quote` to inject some code into each view; one is to `use Phoenix.HTML` which also provides HTML safety


### Showing a User

* add `VideologueWeb.UserController.show/2` with `%{"id" => id}` for params

* add [show.html.eex](videologue/lib/videologue_web/templates/user/show.html.eex) utilizing `@user` from Assigns

> Controller `UserController` would infer its View name `UserView`; this view would look for templates at `lib/videologue_web/templates/user`

* can nest templates like adding `templates/user/user.html.eex` & inject into `show.html.eex` as

```
<h1>User Details</h1>
<%= render "user.html", user: @user %>
```

> * here View is like a Module and Template its functions; each template in app becomes `render(template_name, assigns)`
>
> * utilizing it can simple respond for specific error messages in `VideologueWeb.ErrorView` by adding something like `def render("401.html", _assigns), do: "Who are you to try this?"`

* by default error view implements `template_not_found/2` callback

* when `render` gets called in Controller it first renders Layout view; then renders actual template in pre-def markup

* each template receive certain special assigns like `@view_module`, `@view_template` which gets rendered from `app.html.eex`

* `@conn` is also available in layouts to access any other Phx helpers


### Wrapping Up

> [chapter-04](./chapter-04.md)

---
