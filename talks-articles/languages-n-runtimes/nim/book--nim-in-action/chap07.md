
## Chapter.07 Building a Twitter Clone

> [code parts](code-samples/chap7twit)

* [jester](https://github.com/dom96/jester) is a microframework based on `Sinatra`, used in book to write this clone


### Architecture

* Features to build: Post messages upto 280chars, Subscribe to a user's posts, Timeline with messages from subscriptions

* Data Model: `User{handle}`, `Message{msg, userId, timestamp}`, `Subscription{user_id, subscribe_to_id}`

* `database` module to fetch data and `renderMain` proc to generate HTML, `renderUser` proc to generate small bit of HTML representing user


### Starting

* set up dir structure, init nimble package `nimble init twit`

* add `"jester >= 0.5.0"` to requires and `skipExt = @["nim"]` to skip it's install

* `nimble install -d` to install required dependencies

* build simple hellow-world jester with below code in `src/twit.nim` & run with `nim c -r src/twit` making it available at `localhost:5000`

```
import asyncdispatch, jester

routes:
  get "/":
    resp "Twit!"
runForever()
```


### Database

* Nim supports MySQL, PostgreSQL and SQLite out of the box via `db_mysql`, `db_postgres` & `db_sqlite` modules

* Can use [norm](https://norm.nim.town/), a nim framework-agnostic ORM with SQLite & PgSQL support. It has `norman` as scaffolder and migration manager.

* Here we'll create [db.nim](./code-samples/chap7twit/src/db.nim) to manage our DB requirements to have an insight into how to do it at core.

* Can write tests for it like [db\_test.nim](./code-samples/chap7twit/test/db_test.nim).


### WebApp Views

* `render..` proc are used to generate HTML, escaping invalid chars could be done by using `htmlgen` module like

```
import htmlgen
proc renderUser(user: User, subs: seq[string]): string =
  return `div`(
    h1(user.username),
    span("subsribed to ", $subs.len, " users")
  )
```

* for bigger HTML content, better to use `filters`

```
#? stdtmpl(subsChar = '$', metaChar = '#')
#import "../database"
#
#proc renderUser*(u: User, subs: seq[string]): string =
#  result = ""
<div id="user">
  <h1>${u.username}</h1>
  <span>${$subs.len}</span>
</div>
#end proc
#
#when isMainModule:
#  echo renderUser(User(username: "alice<>"), @[])
#end when
```

> * `? stdtmpl(..)` is filter definition to allow customize the filter behavior
> * `import ..` to import module, can use relative paths
> * important to ensure all line prefixed with `#`
> * create an ordinary proc to initialize `result` var
> * each line not prefixed with `#` converts to `result.add` in compiler
> * `end proc` delimits procedure, then under `isMainModule` to call that proc with default values

Filters are powerful for customization. All above versions don't escape special characters.
Below it specifies toString paramemter to be overwritten with a new `xmltree.escape`.

```
#? stdtmpl(subsChar = '$', metaChar = '#', toString = "xmltree.escape")
#import "../database"
#import xmltree
#
#proc renderUser*(u: User, subs: seq[string]): string =
#  result = ""
<div id="user">
  <h1>${u.username}</h1>
  <span>${$subs.len}</span>
</div>
#end proc
#
#when isMainModule:
#  echo renderUser(User(username: "alice<>"), @[])
#end when
```

> learn more on [Nim's Filters](: http://nim-lang.org/docs/filters.html.)

#### Developing User View

```
#proc renderUser*(u: User, subs: seq[string], myU: User, mySubs: seq[string]): string =
#  result = ""
<div id="user">
  <h1>${u.username}</h1>
  <span>Following: ${$subs.len}</span>
  #if u.username notin mySubs:
  <form action="follow" method="post">
    <input type="hidden" name="subscribe_to" value="${u.username}">
    <input type="hidden" name="my_id" value="${myU.id}">
    <input type="submit" value="Subscribe">
  </form>
  #end if
</div>
#end proc
```

* Created full user view at [views/users.nim](./code-samples/chap7twit/src/views/user.nim) and test at [test/views\_user\_test.nim](./code-samples/chap7twit/test/views_user_test.nim), generating

```
<div id="user">
  <h1>Alice&lt;&gt;</h1>
  <span>0</span>
</div>

<div id="messages">
    <div>
      <a href="/Alice&lt;&gt;">Alice&lt;&gt;</a>
      <span>2022-12-13T21:13:43</span>
      <h3>Whoa first message</h3>
    </div>
</div>
```

#### Developing the General View

* as in [views/general.nim](./code-samples/chap7twit/src/views/general.nim), `toString` param is not used to `body` isn't escaped by defualt but a new operator `$!` is defined to escape strings

* similarly [views/user.nim](./code-samples/chap7twit/src/views/user.nim) to render users own & other users page; [views/login.nim](./code-samples/chap7twit/src/views/login.nim) to show login page and [views/message.nim](./code-samples/chap7twit/src/views/message.nim) to show create/list messages

> for all views feature tests in [tests/views_~~.nim](./code-samples/chap7twit/test) path


### Controller

* update controller to tie togethter view & db model together with routes, as

```
get "/":
  resp renderMain(renderLogin())
```

* can place `style.css` under `public` dir to be picked by default or set via `setStaticDir`

* now implementing `/login` route setting the cookie and redirecting to `/`; update `/` route to render based on cookie key

* adding `cond '.' notin @"username"` at top for route `/usr/@username` shall skip this route for any path where username is passed with `.`


### Deploying

* `jester` isn't a mature server from security perspective; thus recommended to be used behind a Web Proxy like NginX


---
