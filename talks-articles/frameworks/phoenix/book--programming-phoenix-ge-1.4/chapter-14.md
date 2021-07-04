
## Chapter.14 What's Next

### Other Interesting Features

#### Supporting Internationalization with Gettext

* Gettext is there for `i18n` internationalization & `l10n` localization support

* can auto-extract translations from code

> `VideologueWeb.Gettext` gets generated on Phoenix project creation; used in `error_helper.ex`

* different language translations are in `priv/gettext` dir; with pre-set Ecto file `errors.pot` & translations in `priv/gettext/en/LC_MESSAGES`

#### Intercepting on Phoenix Channels

* a broadcast message, Phoenix sends to PubSub system sent to all sockets (this is `fastlaning` since bypasses channels)

* Phoenix Channels provide `intercept` feature allowing channels to intercept broadcast message

* for each `intercept`-ed event, a `handle_out` clause shall be defined to handle event

* Phoenix would send message to all sockets, each channel processing intercepted message

* useful while evolving code, with performance price

#### Understanding Phoenix Live Reload

* used to propagate live changes; with a channel receiving filesystem events & a plug injecting live-reload iframe on every request

#### Phoenix PubSub Adapter

* uses Erlang by default ensuring broadcast works across multiple nodes, requires all machines to be connected as per Erlang Distribution Protocol

* supports multiple adapters like `Redis Adapter`

#### Phoenix Clients for Other Platforms

* Phoenix Channels are using WebSockets, working with ES6 & other platforms

* Channels are Transport Agnostic; can use Custom Protocol to support special requirement


### Phoenix LiveView

> is a lib for building rich interactive bi-directional apps without writing custom JS; [example](https://github.com/chrismccord/phoenix_live_view_example)
>
> * represent a webpage as a function over web state
>
> * establishes messages and callbacks to change state
>
> * allows browser events such as MouseClick, FormSubmit, KeyPress to send events

#### Handling Links in a Counter

* in `router.ex`

```
import Phoenix.LiveView.Router
##
live "/counter", CounterLive
```

* add `lib/some_web/live/counter.ex`

```
defmodule SomeWeb.CounterLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~L"""
    <div>
      <h1>count: <%= @val %></h1>
      <button phx-click="boom" class="alert-danger">BOOM</button>
      <button phx-click="dec">-</button>
      <button phx-click="inc">+</button>
    </div>
    """
  end

  def mount(_session, socket), do: {:ok, assign(socket, :val, 0)}

  def handle_event("inc", _, socket), do: {:noreply, :val, &(&1 + 1)}

  def handle_event("dec", _, socket), do: {:noreply, :val, &(&1 - 1)}
end
```

#### Validating Forms

* with core functions like following, assuming form for new user

```
...
  def render(assigns) do
    Phoenix.View.render(SomeWeb.UserView, "new.html", assigns)
  end

  def mount(_session, socket) do
    {
      :ok,
      assign(socket, changeset: Accounts.change_user(%User{}))
    }
  end

  def handle_event("save", %{"user" => user_params}, socket) do
    Accounts.create_user(user_params) |> do_save()
  end
  defp do_save({:ok, user}) do
    {:stop, socket
            |> put_flash(:info, "user created")
            |> redirect(to: Routes.live_path(socket, UserLive.show, user))}
  end
  defp do_save({:error, %Ecto.changeset{} = changeset}) do
    {:noreply, assign(socket, changeset: changeset)}
  end
...
```


### Phoenix PubSub 2.0

* custom adapters could be provided to use

* PubSub doesn't start as part of Endpoint anymore, need to explicitly provide in Supervision Tree


### Phoenix and Telemetry Integration

* provides Telemetry Developers a unified API for metrics dispatch & instrumentation

* provides a way collecting built-in VM metrics

* gives a unified theme for consuming & reporting metrics

> * [Telemetry](https://github.com/beam-telemetry/telemetry), a dynamic dispatching lib for metrics & instrumentation
>
> * [Telemetry Poller](https://github.com/beam-telemetry/telemetry), allows periodically collection of measurements and dispatch of events
>
> * [Telemetry Metrics](https://github.com/beam-telemetry/telemetry_metrics), provides common interface defining metrics
>
> * [Telemetry Registry](https://github.com/beam-telemetry/telemetry_registry), for event declaration, discovery & registration
>
> [talk: ElixirConf 2020](https://www.youtube.com/watch?v=6GT0UwIVkQI)

### Good Luck!

> [Keep Practicing](https://phoenixframework.org/blog)

---
