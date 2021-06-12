
## Chapter.8 Testing MVC

> like app logic, must be Fast, Isolated, DRY, Repeatable
>
> ensure your test config are as you need, [test.exs](videologue/config/test.exs)

### Understanding ExUnit

> `ExUnit.start` ensures it's ready, placed in [TestHelper]()

* structure

```
defmodule MyModuleTest do
  use ExUnit.Case, async: true

  setup, do: :ok

  test "#myaction" do
  end
...
```

* to run `mix test`; `mix test my/module_test.exs:6` for specificity

#### Creating Test Data

* add `user_fixtures/1` & `video_fixture/1` at [test\_helper.exs](videologue/test/test_helper.exs)


### Testing Contexts

* Phoenix generates a module [test/support/data\_case.ex](videologue/test/support/data_case.ex) serving a foundation for tests that interact with DB

* `data_case` handles setup/teardown of DB integrating with `Ecto.Sandbox` allowing concurrent transactional tests (transactional tests run tests then rollback changes made during test)

* add `import Videologue.TestHelpers` to `using do .. end` 

#### Testing User Accounts

* add [accounts\_test.exs](videologue/test/videologue/accounts_test.exs) for `register_user/1` with different input validities

> add `, async: true` to `use Videologue.DataCase`

```
defmodule Videologue.AccountsTest do
  use Videologue.DataCase, async: true

  alias Videologue.Accounts
  alias Videologue.Accounts.User

  describe "register_user/1" do

    @valid_attrs %{username: "testuser", name: "test user", password: "cryptography"}
    @invalid_attrs %{}

    test "with valid data inserts" do
      assert {:ok, %User{id: id} = user} = Accounts.register_user(@valid_attrs)
      assert user.name == "test user"
      assert user.username == "testuser"
      assert [%User{id: ^id}] = Accounts.list_users()
    end
  ...
...
```

> just run this specific test set as `mix test test/videologue/accounts_test.exs`

* add tests for `authenticate_by_username_and_password/2` with `setup` block using `user_fixture` pre-set for password

* while adding tests for Category in my version, I noticed I haven't added `changeset` flow for `create_categories!/1` which allows nullable names; so fixed that


### Using Ecto Sandbox for Test Isolation and Concurrency

* one of the features provided by `DataCase` is `Ecto Sandbox`; its role is to do DB transaction rollbacks

> * it just wraps each test in a transaction, then rollbacks.. enabling concurrent testing with same DB
>
> * needing `aync: true` tag


### Integration Tests

> test the route via endpoint, as a real web request does making sure Controller returns success/redirect/error-codes as desired

#### Warming Up with PageController

* checkig [PageController](videologue/test/videologue_web/controllers/page_controller_test.exs) shows `use VideologueWeb.ConnCase` that adds support of [conn\_case.ex](videologue/test/support/conn_case.ex)

> if using **PostgreSQL**; even here `ConnCase` can be passed `async: true`; not with all DBs
>
> * uses `Phoenix.ConnTest` to set up API, import convenient aliases
>
> * ensure to be tested Endpoint is configured
>
> * `setup` here return `conn` if `:ok`
>
> * assertion helpers like `html_response/2`, `json_response/2` are available
>
> add `import Videologue.TestHelpers` to `using` macro in `ConnCase`; making fixture functions available in controller tests

#### Testing Logged-out Users

* add following for non-auth causing redirects, and run Mix tests by line-number on this script to just check this

```
  describe "logged-out user" do
    test "gets redirected for all video actions", %{conn: conn} do
      [
        get(conn, Routes.video_path(conn, :new)),
        get(conn, Routes.video_path(conn, :index)),
        get(conn, Routes.video_path(conn, :show, "101")),
        get(conn, Routes.video_path(conn, :edit, "101")),
        get(conn, Routes.video_path(conn, :update, "101", %{})),
        get(conn, Routes.video_path(conn, :create, %{})),
        get(conn, Routes.video_path(conn, :delete, "101")),
      ] |> Enum.each(fn conn ->
        assert html_response(conn, 302)
        assert conn.halted
      end)
    end
  end
```

#### Preparing for Logged-in Users

> * don't embed `user_id` is session for `Auth` plug to be tricked by, as that could cause leaky implementation
>
> direct request to session controller everytime would be expensive

* test login mechanism in isolation; build a bypass for rest usecases

> add following pat-match for `call` in [auth.ex](videologue/lib/videologue_web/controllers/auth.ex) to ensure if a `current_user` exists it's honored

```
def call(conn, _opts) when not is_nil(conn.assigns.current_user), do: conn
```

> now tests for logged-in users are much cleaner

#### Testing Logged-in Users

* adding `setup %{conn: conn, login_as: username}, do: login(conn, username)` like prep functions to make session have user-id for relevant video

#### Using Tags

* now to provide `login_as: username` construct to `setup`; for each test used with it add `@tag login_as: "alice"`

* tag module attribute accepts keyword-list/atom; providing atom is a way to provide flag style attribs

* can also be used to filter tests as `mix run test/videologue_web --only login_as`

* had to `{:ok, conn} = recycle(conn) |> conn_assign(user)` before every later HTTP request in tests; else it lost user-id in Conn Assigns

* need to cover negative flows which have been managed for explicitly; so to ensure they don't break.. like access of other user's content

> `Ecto.NoResultsError` gets treated by `Plug` as `404` hence test-able via `assert_error_sent`


### Unit-Testing Plugs

> should test authentication code now, as getting bypassed in Integration Test

* [AuthTest](videologue/test/videologue_web/controllers/auth_test.exs) uses `setup` to `bypass_through/3` as `authenticate_user` puts flash & `:browser` pipeline in router plugs `fetch_flash` to set it up

> * our Unit Test depends on other functionality like `fetch_flash`; here test-helper `bypass_through/3` allows to do a request that goes via entire pipeline but bypasses router dispatch
>
> * performing `get` on `bypass_through` accesses endpoint and stops at browser pipeline; here path provided isn't used by Router just stored in connection

* now we keep this `setup` for all functions, as without it tests for `login/logout` will error for `session not fetched`... which gets prepared by our `bypass_through |> get`

* adding tests for `call` is simple to check if a placed user id in session gets picked into Assigns

* test time can be reduce by tweaking hashing of passwords for tests only `config :pbkdf2_elixir, :rounds, 1` to `config/test.exs`


### Testing Views and Templates

* since Phoenix templates are just functions in Parent's View modules, can test like any other


### Wrapping Up

> Part.II [Chapter-09 Watching Videos](./chapter-09.md)

---
