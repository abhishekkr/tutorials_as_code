
## Chapter.1 Introducing Phoenix

### Productive

* gets you started quickly with help of boilerplate architecture for app; dbms access; routing layer; HTML templating; JSON en/decoding; i18n startegies; Power of Erlang

* favors convention over configuration; layers complexity instead of hiding by breaking functionality into small functions & well-named modules

* Elixir got Immutability; data structures don't change, thus layers of small functions; thus no spillover state & code reliability


### Concurrent

* BEAM provides I/O concurrency without callbacks (used in some languages to avoid state leaks) with user-space multi-core concurrency

> * write straighforward code, Erlang VM takes care of CPU & I/O resource utilization
>
> * Bleacher Report was able to replace 150 RoR instances with 5 Phoenix

* immutable data helps avoid concurrency leaks; excpet race condition natural to domain

### Beautiful Code

* web servers need to map routes ontp functions that do job

```
pipeline :browser do
  plug :accepts, ["html"]
  plug :fetch_session
  plug :protect_from_forgery
end

pipeline :api do
  plug :accepts, ["json"]
end

scope "/", Blah do
  pipe_through :browser
  get "/blog", BlogController, :index
  ..
end

...
```

* using inheritance chains, building pipeline for group of routes working same way.. reducing router to pattern matching

> Ecto's elegant query syntax; composing requests as function pipeline


### Interactive

* Elixir helps create 100s of 1000s of lightweight processes that also mean that many connections/conversations with channels

* a typical channels feature might look like

```
def handle_in("new_annotation", param, socket) do
  boradcast! socket, "new_annotation", %{
    user: %{username: "guest"},
    body: params["body"],
    at: params["at"]
  }
  {:reply, :ok, socket}
end
```

* very few other stack come with guarantee of isolatio and high concurrency for channels

* Phoenix can vertically scale by connections tested with 2mil on a node; also horizontally running a Phoenix cluster boradcasting messages across all nodes out of the box

* tracks Presence of which users are connected regardless where they happen in cluster

* LiveView allows real-time applications without custom JavaScript with something as following

```
defmodule Demo.CounterLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~L"""
    <span><%= @val %></span>
    <button phx-click="inc">+</button>
    """
  end

  def mount(_session, socker) do
    {:ok, assigng(socket, val: 0)}
  end

  def handle_event("inc", _, socket) do
    {:noreply, update(socket, :val, &(&1 + 1))}
  end
end
```

> when rendered first time, Phoenix connects to LiveView on server via WebSockets & Channels.. LiveView is smart to henceforth send only changes

* state can hold whatever needed; events changing state can be any


### Reliable

* secret sauce of Process linking architecure & Process communication; Elixir's Supervisor tree

> can monitor and introspect the connection pools
>
> strong, clean foundation of Erlang for Relaibility is baked in

---
