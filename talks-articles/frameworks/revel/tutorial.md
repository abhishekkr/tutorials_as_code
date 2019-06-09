
## Basic Tutorial

> quickstart guide

* get revel

```
go get github.com/revel/cmd/revel
```

* create a new webapp project

```
revel new github.com/abhishekkr/prajapati/manager
```

* run server using following command, listen at `:9000` by default

```
revel run -a github.com/abhishekkr/prajapati/manager
```

* generated project structure is described in [organization](https://revel.github.io/manual/organization.html)

* HTTP port config in [conf/app.conf](https://revel.github.io/manual/appconf.html#httpport)

* additional revel commands at [tool doc](https://revel.github.io/manual/tool.html)

* detials on routing config at [doc/routing](https://revel.github.io/manual/routing.html)

* `GET / App.Index` in `conf/routes` tells Revel to invoke `App` [controller](https://revel.github.io/manual/controllers.html) for `GET` request at `/`

```
//  app/controllers/app.go:

package controllers

import "github.com/revel/revel"

type App struct {
    *revel.Controller
}

func (c App) Index() revel.Result {
    return c.Render()
}
```

* Controller must be a struct that embeds `*revel.Controller` in first slot. Any mehtod on Controller returns `revel.Result`. In default route `App.Index` is `Action` used for `/`.

* Revel controller provides multiple methods for generating result, like `Render()` that finds a [template](https://revel.github.io/manual/templates.html) for response.

* [Templates](https://revel.github.io/manual/templates.html) are in `app/views/<Controller>/<Action>.html` directory. Additional to Go's template function, few [extra](https://revel.github.io/manual/templates.html#functions) are available.

* Revel has watchers checking for change and hot-reloading. Paths managed at `config/app.conf`, default paths are `app/`, `app/views` and `config/routes`.

* Following changes will make data available in view

```
// update Action to pass data to Render
...
func (c App) Index() revel.Result {
	user := os.Getenv("USER")
	return c.Render(user)
}

// update Action view to display it
...
<div class="container">
  <div>hey @{{.user}}</div>
...
```

---
