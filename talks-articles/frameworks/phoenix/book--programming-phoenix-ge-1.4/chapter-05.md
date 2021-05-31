
## Chapter.5 Authenticating Users

### Preparing for Authentication

* Auth is one of the critical features, leaks in which can tumble entire Orgs

> * Users will provide traditional creds while Registration; will be stored safely in DB.
>
> * Sessions will be used with info of User; will have Users marked authenticated on giving correct creds.

* [comeonin](https://github.com/riverrun/comeonin) is a spec for password hashing libraries; we'll use [Pbkdf2](https://github.com/riverrun/pbkdf2_elixir) password hashing as it doesn't need native bindings alongwith being resistant to Dictionary/Rainbow attacks

> there is a library maintained by same contributor called [phxauth](https://github.com/riverrun/phauxth) managin Auth for Phoenix & Plug-based Webapps, [Getting Started](https://github.com/riverrun/phauxth/wiki/Getting-started)

* add `{:pbkdf2_elixir, "~> 1.4"}` to `mix.exs`; then `mix deps.get`


### Managing Registration Changesets

* we'll use one changeset per use-case; Chapter-04's Changeset handle all attributes yet, except Passwords

* build another changeset for sensitive data case as credential changes

* add 2 more fields to Schema, `:password` (not stored, since plaintext) & `:password_hash` (will be added to DB)

> `virtual: true` marks Struct only fields for Ecto, which are not to be persisted in backed DB

* add [Videologue.Accounts.User.registration\_changeset/2](videologue/lib/videologue/accounts/user.ex.chapter05) to handle user registrations

> here `registration_changeset/2` resuses `changese/2` as first Pipe; then `cast -> validate_required -> validate_length` the password; then pushes Changeset to `put_pass_hash/1` which persists Hash if Password validated else returns Changeset with errors

* in projects for real use, shall use libs/measures to ensure Passwords are stronger

> add a [new migration](videologue/priv/repo/migrations/20210528191400_add_users_field_password_hash.exs) to provide a temp-pass for all existing users


### Creating Users

* keep [Videologue.Accounts](lib/videologue/accounts.ex) `create_user` to manage any workflow requiring it

* add `Videologue.Accounts.register_user/1` & `change_registration/2` managing a/c registration with passwords; just using `registration_changeset` instead of `changeset`

* update [VideologueWeb.UserController](videologue/lib/videologue_web/controllers/user_controller.ex.chapter05) `new` to use `change_registration/2` and `create` to use `register_user/1`

* update [new.html.eex](videologue/lib/videologue_web/templates/user/new.html.eex) to have a new `div` for Password field

> now we need an Auth Service, make it available throughout Pipelines via Plugs


### The Anatomy of a Plug

* typical Plug take a conn, return a conn

* 2 kinds of Plugs: `module plugs` (providig 2 functions `init/1` & `call/2` with config details) and `function plugs` (a single function)

> `endpoint.ex` usage of `plug Plug.RequestId` is a module plug; `router.ex` usage of `plug :accepts, ["json"]` is a function plug

#### Module Plugs

* used when to share a plug across modules; spec need it to have `init/1` & `call/2`

```
defmodule ThisPlug do
  def init(opts), do: opts
  def call(conn, _opts), do: conn
end
```

* uses result of `init` as 2nd arg to `call`.. in PROD `init` runs only once at compile time; during DEV it runs compile time

> Having common Data Structure piped through a chain as input/output is Key. Right Common Data Structure here is key.

#### Plug.Conn Fields, [hexdocs](http://hexdocs.pm/plug/Plug.Conn.html)

> Cowboy (default Phoenix web-server) parses inbound request, which contains string except where otherwsie specified

* `host` and `method` are obvious; `path_info` is List of path segments; `req_headers` is list of tuples for headers; `scheme` is Protocol as atom

* there are fetchable fields in Conn as well (empty until called for to save process) like

> * `cookies` are request/response cookies
>
> * `params` are request params; some plugs help parse these from query string or request body

* some fields are used to process web request & keep info of plug pipeline

> * `assigns`, user-defined map for whatever
>
> * `halted`, connection might be halted in cases like auth fail

* can also find a `secret_key_base` for encryption

* Response fields `resp_body`, `resp_cookies`, `resp_headers`, `status`

* Some private fields reserved for adapter/frameworks: `adapter` is underlying web-server, `private` field has map for private use of frameworks


### Writing an Authentication Plug

* auth works in 2 stages

> * store user-id in session when new user registers/a user logs in
>
> * check if a new user is in session; store in `conn.assigns` for every incoming so accessible in controllers & views

* create `VideologueWeb.Auth` plug via `import Plug.Conn` at [auth.ex](videologue/lib/videologue_web/controllers/auth.ex) adding `user` details to Assigns for `user_id` in current session

* add this Auth plug in browser pipeline

#### Restricting Access

* let's disable access to `:index` & `:show` in `VideologueWeb.UserController` unless logged in

* if we add `authenticate/1` private function; using it as following in `index/2` (similarly for `show`) would work fine

```
  def index(conn, _params) do
    case authenticate(conn) do
      %Plug.Conn{halted: true} = conn ->
        conn
      conn ->
        users = Accounts.list_users()
        render(conn, "index.html", users: users)
    end
  end

  ...

  defp authenticate(conn) when is_nil(conn.assigns.current_user) do
    conn
    |> put_flash(:error, "You are not logged in.")
    |> redirect(to: Routes.page_path(conn, :index))
    |> halt()
  end
  defp authenticate(conn) do
    conn
  end
```

* that would be repititive, bloated & error prone; let's Plug the `authenticate/2` function by also passing `_opts` as param and `plug :authenticate when action in [:index, :show]`

> with Plug we don't need to update the `index` or `show` action; since Plug pipeline explicitly itself checks for `halted: true` in Connection between Plug calls

#### Logging In

* add `VideologueWeb.Auth.login/2` which update Assigns for `current_user`, puts `user.id` in session info & configures session renew to `true` avoiding Session Fixation attacks

* add Pipe for this `login` to `UserController.do_create(conn, {:ok, user})` for Session to be updated and log-in to happen on user creation


### Implementing Login and Logout

* add `Videologue.Accounts.authenticate_by_username_and_password/2` to auth from DB data

> `Pbkdf2.no_user_verify()` gets used to avoid Timing Attack by simulating a password check with variable timing

* add `sessions` resource to Routes calling for [SessionController](videologue_web/lib/videologue_web/controllers/session_controller.ex) for `:new`, `:create` and `:delete` actions

> add a [session view](lib/videologue_web/views/session_view.ex) for Templates to get a home

* `:new` to just render [new.html.eex](videologue/lib/videologue_web/templates/session/new.html.eex) which shows a Log-in form if no user is logged in, else just info of logged in user and a link to log-out to log back in


### Presenting User Account Links

* add Log-out link to `app.eex` layout as well; alongwith current-user if logged-in otherwise a Log-in/Register link

> we're passing `delete` HTTP method for Log-out link, which makes `link` to generate a form tag instead of anchor tag... method defaults to GET

* add `VideologueWeb.Auth.logout/1` to `drop` session

* add `VideologueWeb.SessionController` for `delete` action using this `logout`

> we have a full working Log-in/out and Sign-up flow now


### Wrapping Up

> [Chapter-06 Generators and Relationships](./chapter-06.md)

---
