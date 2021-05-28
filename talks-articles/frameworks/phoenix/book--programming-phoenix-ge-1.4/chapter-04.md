
## Chapter.4 Ecto and Changesets

> now we can replace our hard-coded data with a real DB without changing controller code

* Ecto is Elixir framework for data management; here we'll update Accounts context to use Ecto backed by PgSQL

### Understanding Ecto

* is a wrapper primarily intended for RDBMS

* has encapsulated Query lang used to build layered queries

* has `changesets` holding all changes desired to perform on Records encapsulating receiving external data, casting & validating it

* by default we have [repo.ex](videologue/lib/videologue/repo.ex) generated Ecto Repository alongwith config for connection to default PgSQL adapter at `./config/` under `config :videologue, Videologue.Repo, ..`

* `mix ecto.create` creates the DB schema if not already there; `mix ecto.migrate` to apply migrations


### Defining the User Schema and Migration

* can specify Struct with individual fields tied to DB:Table fields through a DSL

* update [Videologue.Accounts.User](videologue/lib/videologue/accounts/user.ex) bare user struct to define Schema

> DSL used for Schema is built with `schema` & `field` Macros; allowing us to define underlying DB:Table & Elixir Struct together

* now generate a migration for DB to reflect the same structure `mix ecto.gen.migration create_users` which generates a [migration file](priv/repo/migrations/20210527193258_create_users.exs) with timestamp; let's add following to its `change/0` action

```
..
  def change do
    create table(:users) do
      add :name, :string
      add :username, :string, null: false
      add :password_hash, :string

      timestamps()
    end
  end
```

* then generate a migration to add index to `users`, we could have added this to earlier migration but it's just a better practice to have seemingly single step migrations

```
mix ecto.gen.migration create_users_index_username
* creating priv/repo/migrations/20210527193448_create_users_index_username.exs
```

> add following to `change` here `create unique_index(:users, [:username])`

> * here we used `create` & `add` macros available via `use Ecto.Migration` API
>
> * `timestamps()` add auto-generated `inserted_at` & `updated_at` fields

* let's migrate DB with `mix ecto.migrate`

* Repo are ready to use through Accounts context; Repository service need to be up & running... Phoenix ensures that by having it in `child_spec` list under [application.ex](videologue/lib/videologue/application.ex)


### Using the Repository to Add Data

* can add data using IEx as

```
iex(1)> alias Videologue.Repo
Videologue.Repo

iex(2)> alias Videologue.Accounts.User
Videologue.Accounts.User

iex(3)> %User{username: "sherlock", name: "Sherlock Holmes"} |> Repo.insert()    
[debug] QUERY OK db=2.5ms decode=1.7ms queue=0.8ms idle=377.5ms
INSERT INTO "users" ("name","username","inserted_at","updated_at") VALUES ($1,$2,$3,$4) RETURNING "id" ["Sherlock Holmes", "sherlock", ~N[2021-05-28 06:51:43], ~N[2021-05-28 06:51:43]]
{:ok,
 %Videologue.Accounts.User{
   __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
   id: 1,
   inserted_at: ~N[2021-05-28 06:51:43],
   name: "Sherlock Holmes",
   updated_at: ~N[2021-05-28 06:51:43],
   username: "sherlock"
 }}

iex(4)> %User{username: "moriarty", name: "James Moriarty"} |> Repo.insert() 

iex(5)> %User{username: "watson", name: "John Watson"} |> Repo.insert()     

iex(6)> Repo.all(User) |> Enum.filter(fn x -> x.username =~ "lock" end) 
[debug] QUERY OK source="users" db=8.6ms queue=1.6ms idle=1886.3ms
SELECT u0."id", u0."name", u0."username", u0."inserted_at", u0."updated_at" FROM "users" AS u0 []
[
  %Videologue.Accounts.User{
    __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
    id: 1,
    inserted_at: ~N[2021-05-28 06:51:43],
    name: "Sherlock Holmes",
    updated_at: ~N[2021-05-28 06:51:43],
    username: "sherlock"
  }
]

iex(7)> Repo.get(User, 2)                                              
[debug] QUERY OK source="users" db=2.4ms queue=2.2ms idle=1155.0ms
SELECT u0."id", u0."name", u0."username", u0."inserted_at", u0."updated_at" FROM "users" AS u0 WHERE (u0."id" = $1) [2]
%Videologue.Accounts.User{
  __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
  id: 2,
  inserted_at: ~N[2021-05-28 06:52:11],
  name: "James Moriarty",
  updated_at: ~N[2021-05-28 06:52:11],
  username: "moriarty"
}
```

* can update `Videologue.Accounts` with `Repo.all/1`, `Repo.get/2`, `Repo.get!/2`, `Repo.get_by/2` replacing hard-coding from `alias Videologue.Repo`


### Building Forms

> use Phoenix form builder to enable new user creation

* add new user account action to [VideologueWeb.UserController](videologue/lib/videologue_web/controllers/user_controller.ex)

* add `changeset/2` function to [Videologue.Accounts.User](lib/videologue/accounts/user.ex) doing `cast/2`, `validate_required/1` & `validate_length` imported from Schema module

* add `change_user/1` function to `Videologue.Accounts.User` which gets utilized in our `new` action in Controller

* `Videologue.Accounts` module itsef shall be only public API our Controllers shall call

* way of piping in required validations enables flexible stream

* in [router.ex](lib/videologue_web/router.ex), add `resources "/users", UserController, only: [:index, :show, :new, :create]` replacing other `/users...` routes

* doing `mix phx.routes` gives fillowing routes relevant to us

```
user_path  GET   /users           VideologueWeb.UserController :index
user_path  GET   /users/new       VideologueWeb.UserController :new
user_path  GET   /users/:id       VideologueWeb.UserController :show
user_path  POST  /users           VideologueWeb.UserController :create
page_path  GET   /                VideologueWeb.PageController :index
```

> not limiting resources with `only:` will give couple more routes for PUT, PATCH, DELETE, etc.

* add [new.html.eex](videologue/lib/videologue_web/templates/user/new.html.eex); here using Form Builder to generate required fields in association with Model struct

> * this introduces a CSRF token for input fields; add proper IDs, Names to them
>
> * available at [/users/new.html](http://127.0.0.1:4000/users/new.html)


### Creating Resources

* our Form submits data for new user and expects `VideologueWeb.UserController.create/2`

* add `Videologue.Accounts.create_user/1` to do `Repo.insert`; then add `VideologueWeb.UserController.create/2` to call this and redirect to Listing on success else render errors on create page if any

> * `{:ok, user}` gets returned on successful Repo insert; `{:error, changeset}` on erroneous with validation errors
>
> * `:action` field of changeset indicates action tried to perform on it; rendering form with any action we know it had validation errors
>
> * `error_tag` gets used in form to display these errors at frontend

* in addition to validation errors; Changesets track changes; lets see some IEx work on it

```
iex(1)> alias Videologue.Accounts.User
Videologue.Accounts.User

iex(2)> changeset = User.changeset(%User{username: "hudson", name: "Mrs Hudson"}, %{})
#Ecto.Changeset<action: nil, changes: %{}, errors: [],
 data: #Videologue.Accounts.User<>, valid?: true>

iex(3)> import Ecto.Changeset
Ecto.Changeset

iex(4)> cset = put_change(changeset, :username, "mrshudson")
#Ecto.Changeset<
  action: nil,
  changes: %{username: "mrshudson"},
  errors: [],
  data: #Videologue.Accounts.User<>,
  valid?: true
>

iex(5)> cset.changes                                                                  
%{username: "mrshudson"}

iex(6)> get_change(cset, :username)     
"mrshudson"

iex(7)> get_change(cset, :name)    
nil
```


### Wrapping Up

> [chapter-05](./chapter-05.md)

---
