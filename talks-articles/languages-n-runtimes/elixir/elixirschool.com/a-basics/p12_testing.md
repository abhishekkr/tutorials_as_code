
## Testing

### ExUnit

* is Elixir's built-in test framework, tests are implemented in elixir scripts using `*.exs` file extension

* ExUnit can be started using `ExUnit.start()`, most commonly done in `test/test_helper.exs`

* `mix test` run project's test

#### assert

* using `assert` macro to test that expression is true, test for example module in `example_test.go`

```
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  test "greets the world" do
    assert Example.hello() == :word
  end
end
```

#### refute

* `refute` is negate of `assert`, so for unless case of assert

#### assert\_raise

* to assert that an error will be raised, use `assert_raise`

#### assert\_receive

* app consists of actor/processes that send messages to each other, these get used for cases of process receiving messages

* `assert_received` can be used without waiting for messages, `assert_receive` can specify timeout

```
defmodule SendingProcess do
  def run(pid) do
    send(pid, :ping)
  end
end

defmodule TestReceive do
  use ExUnit.Case

  test "receives ping" do
    SendingProcess.run(self())
    assert_received :ping
  end
end
```

#### capture\_io and capture\_log

* capturing an application's output is possible with `ExUnit.CaptureIO`, can be tested as

```
import ExUnit.CaptureIO

assert capture_io(fn -> IO.puts("Hey") end) == "Hey\n"
```

* `ExUnit.CaptureLog` similarly for capturing output to `Logger`


### Test Setup

* some tests need setup, `setup` and `setup_all` macros can perform same; they are to return `{:ok, state}` where `state` is available to all tests

```
## example
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  setup_all do
    {:ok, recipient: :someone}
  end

  tests "greets", state do
    assert Example.hey() == state[:recipient]
  end
end
```


### Mocking

* [mox](https://github.com/plataformatec/mox) library defining concurrent mocks in Elixir, as per Jose Valim's ideas

* **DO NOT** mock in Elixir, they are discouraged. Use Mock implementations in client code for testing.

* Switch implementation in app code, passing module as argument and using a default value. For creating mock implementations, can use behaviors and callbacks.

* Jose Valim's detailed thoughts are available [here](http://blog.plataformatec.com.br/2015/10/mocks-and-explicit-contracts/).

---
