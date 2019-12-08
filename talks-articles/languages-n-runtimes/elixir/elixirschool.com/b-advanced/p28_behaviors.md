
## Behaviors

> [sample code](./p28_behaviors.exs)

* elixir behaviors are requiring a module to implement type specifications; these perform 2 primary roles

> * defining a function set to be implemented
>
> * checking if the set was actually implemented

* one of the popular elixir behavior is `GenServer`


### Defining a behavior

* `ExampleBehavior.Worker` in sample code, a worker module behavior for 2 functions `init/1` and `perform/2`, utilizing `@callback` directive

* `@callback` declaration is similar to `@spec`, defines required function

* `@macrocallback` gets used for macros

* example

```
defmodule Some.Behavior do
  @callback foo(arg :: term, state :: term) :: {:ok, result :: term} | {:error, reason :: term}
end
```


### Using a behavior

* `ExampleBehavior.Consumer` and `ExampleBehavior.Compressor` in sample code use `@behaviour` attribute to impose the `ExampleBehavior.Worker` public api onto them

* if any of the intended `@callback` or `@macrocallback` is missed in imposed modules, raises a **warning** of `undefined behavior` on compile

---
