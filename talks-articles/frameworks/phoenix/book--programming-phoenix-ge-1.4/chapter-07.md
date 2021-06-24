
## Chapter.7 Ecto Queries and Constraints

### Seeding and Associating Categories

* Phoenix has a convention for seeding data, at [seeds.exs](videologue/priv/repo/seeds.exs). Use Repo to directly add the data we need, then use mix commands.

* Seed script should be capable of not generating duplicates, as might run multiple times.

> * could use something like `Repo.get_by(Category, name: name) || Repo.insert!(%Category{name: name})`; although this doesn't take care of Race conditions of being run by multiple users simultaneously

* [Upsert](https://hexdocs.pm/ecto/Ecto.Repo.html#c:insert/2-upserts) is an acceptable tactic; we'll do `on_conflict: :nothing` which is `:raise` by default

> * add `create_category!/1` to [Videologue.Multimedia](videologue/lib/videologue/multimedia.ex.chapter09)
>
> * add default category seed script to `seeds.exs` & execute `mix run priv/repo/seeds.exs`

#### Associating Videos & Categories

> fetch all category names+ids; sort by name, pass them to view via Select input

* using `import Ecto.Query` allows using macros like `from`, formulate queries like `from c in Category, select: c.name` and pass it to `Repo.all` for a custom query

* Ecto Queries are composable as below 2 chunks are similar in action

```
Repo.all from c in Category,
         order_by: c.name,
         select: {c.name, c.id}
```

```
query = Category
query = from c in query, order_by: c.name
query = from c in query, select: {c.name, c.id}
Repo.all(query)
```

* composable form works due to Ecto's `queryable` protocol which acts as base for every new query; allowing usage of Pipes to create Complex queries from Simple

> * add `alphabetical/1` to [Videologue.Multimedia.Category](videologue/lib/videologue/multimedia/category.ex) adding `from c in query, order_by: c.name` as queryable and updating query
>
> * add `list_alphabetical_categories/0` to [Videologue.Multimedia](videologue/lib/videologue/multimedia.ex.chapter09) using `alphabetical/1`
>
> * add `category_select_options/1` to [VideologueWeb.VideoView](videologue/lib/videologue_web/views/video_view.ex) mapping categories of Assigns sent to it as params into  `{name, id}` tuple list
>
> * call this `category_select_options` in [form.html.eex](lib/videologue_web/templates/video/form.html.eex) as value to `select` field for category-id
>
> browse New/Edit, and select to choose category is there now


### Diving Deeper into Ecto Queries

* when formulating Queries use `^variable` for injecting value/expression for query interpolation

```
iex(1)> import Ecto.Query
iex(2)> username = "sherlock"

iex(3)> Videologue.Repo.one(from u in Videologue.Accounts.User, where: u.username == username)
** (Ecto.Query.CompileError) unbound variable `username` in query. If you are attempting to interpolate a value, use ^var
    (ecto 3.6.1) expanding macro: Ecto.Query.where/3
    iex:3: (file)
    (ecto 3.6.1) expanding macro: Ecto.Query.from/2
    iex:3: (file)

iex(3)> Videologue.Repo.one(from u in Videologue.Accounts.User, where: u.username == ^username)
%Videologue.Accounts.User{
  __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
  id: 1,
  inserted_at: ~N[2021-05-28 06:51:43],
  name: "Sherlock Holmes",
  password: nil,
  password_hash: "$pbkdf2-sha512$160000$UGmpw3Qld5JgX6Cv49VNAQ$.m8nqJ.VLyCHulruhlIOr6ic2xUNjNVg4T1CWnxTVOLBuCF3/l5KXuN4KHySFs1D46ORCVBbsuzbkv4UdyQuKA",
  updated_at: ~N[2021-05-28 06:51:43],
  username: "sherlock"
}
```

* Ecto queries do normalization at compile time for performance leveraging info in schemas to cast values at runtime; so a wrong type for `username` in above example will give `Ecto.Query.CastError`

* `^var` is interpolated by Ecto scrubbing & safely placing them to use trying minimize SQL-injection risks

* Ecto Query API supports wide operator range, [doc](http://hexdocs.pm/ecto/Ecto.Query.API.html)

> * Comparison: `==, !=, <-, >=, <, >`
>
> * Boolean: `and, or, not`
>
> * Inclusion: `in`
>
> * Search: `like, ilike`
>
> * Null check: `is_nil`
>
> * Aggregates: `count, avg, sum, min, max`
>
> * Date/time intervals: `datetime_add, date_add`
>
> * General: `fragment, field, type`

#### Writing Queries with Keywords Syntax

* moving `Repo.one(from u in User, select: count(u.id), where: ilike(jc.username, ^"j%") or ilike(jc.username, ^"c%"))` to Keywords Syntax

```
users_count = from u in User, select: count(u.id)
j_or_c_users = from jc in users_count, where: ilike(jc.username, ^"j%") or ilike(jc.username, ^"c%")
Repo.one(j_or_c_users)
```

#### Using Queries with Pipe Syntax

```
User
|> select([u], count(u.id))
|> where([u], ilike(jc.username, ^"j%") or ilike(jc.username, ^"c%"))
|> Repo.one()
```

#### Fragments

* Ecto's `query fragment` sends part of query to DB, but allows construct it safely

> to lookup user by username in a case-insensitive way; using a fragment allow construct a SQL fragment for Query & safely interpolate

```
from u in User,
  where: fragment("lower(username) = ?", ^String.downcase(name))
```

* if nothing works, can just run direct SQL using `Ecto.Adapters.SQL.query`

```
Repo |> Ecto.Adapters.SQL.query("SELECT power($1, $2)", [2, 10])
```


#### Querying Relationships

* we don't always need to `Repo.preload` as a separate step; Ecto allows preload associations as part of Query as following

```
video = Videologue.Repo.one(
          from v in Videologue.Multimedia.Video,
            limit: 1,
            preload: [:user]
)
```

* can join on associations inside queries

```
Videologue.Repo.all from v in Video,
         join: u in assoc(v, :user),
         join: c in assoc(v, :category),
         where: c.name == "elixir",
         select: {u, v}
```


### Constraints

* allow use underlying relational DB features to help maintain DB integrity

> like check if Category exist before updating Video for provided category-id 

#### Validating Unique Data

> our migration ensure `username` to be unqiue at DB layer; if breached giving `Ecto.ConstraintError`

* changing Ecto ConstraintError into Changeset error message to allow informing User ina subtle way

* adding `unique_constraint(:username)` to changeset Pipe in [user.ex](videologue/lib/videologue/accounts/user.ex)

> this ensures `has already been taken` error to be shown in the constraint is breached

#### Validating Foreign Keys

* adding `assoc_constraint(:category)` to `Video.changeset` pipes converts Foreign Key constraint error into human-readable error message alongwith guarantee for valid category-id being used

```
iex(1)> import Ecto.Query

iex(2)> video = Videologue.Repo.one(from v in Videologue.Multimedia.Video, limit: 1)

iex(3)> changeset = Videologue.Multimedia.Video.changeset(video, %{category_id: 101})

iex(4)> {:error, changeset} = Videologue.Repo.update(changeset)
[debug] QUERY ERROR db=4.9ms queue=0.6ms idle=1984.5ms
UPDATE "videos" SET "category_id" = $1, "updated_at" = $2 WHERE "id" = $3 [101, ~N[2021-05-31 19:01:10], 1]
{:error,
 #Ecto.Changeset<
   action: :update,
   changes: %{category_id: 101},
   errors: [
     category: {"does not exist",
      [constraint: :assoc, constraint_name: "videos_category_id_fkey"]}
   ],
   data: #Videologue.Multimedia.Video<>,
   valid?: false
 >}
```

* so use Constraints with Changesets to your easy bug-free UX advantage

#### On Delete

* `Repo.Delete` accepts Changesets, can use `foreign_key_constraint` to ensure no Records with to-be deleted Record's key Exist; else provide an error

> like Showing user why they can't delete a Category, if there are Videos related to it
>
> * in case of Category, Foreign Key constraint reside at Video table so need to be explicit in Constraint

```
iex(1)> import Ecto.Changeset

iex(2)> category = Videologue.Repo.get_by(Videologue.Multimedia.Category, name: "erlang")

iex(3)> Videologue.Repo.delete(category)
** (Ecto.ConstraintError) constraint error when attempting to delete struct:

    * videos_category_id_fkey (foreign_key_constraint)
    ...

iex(3)> changeset = change(category)

iex(4)> changeset = foreign_key_constraint(changeset, :videos, name: :videos_category_id_fkey, message: "some videos are using this")
#Ecto.Changeset<action: nil, changes: %{}, errors: [],
 data: #Videologue.Multimedia.Category<>, valid?: true>

iex(5)> Videologue.Repo.delete(changeset)
{:error,
 #Ecto.Changeset<
   action: :delete,
   changes: %{},
   errors: [
     videos: {"some videos are using this",
      [constraint: :foreign, constraint_name: "videos_category_id_fkey"]}
   ],
   data: #Videologue.Multimedia.Category<>,
   valid?: false
 >}
```

* could configure DB references to either `cascade` the deletions or simply make `videos.category_id` to `NULL` on `delete` with a new migration as

```
add :category_id, references(:categories, on_delete: :nilify_all)
```

> * `:nothing` is default to `on_delete`, there is also option `:delete_all` to delete all linked Foreign records
>
> * can have an external call triggered in Pipe, returning Changeset; adding Foreign field mapping to a non-related TABLE mapping for REVIEW in case the relations are sensitive

#### Let It Crash

> if a category is deleted, here shall just show an error to pick something else when loaded

* when nothing can be done by user to fix the error, let it crash with a decent Error Message and landing page; log it to check if something needs fixing or just purging


### Wrapping Up

> [Chapter-08 Testing MVC](./chapter-08.md)

---
