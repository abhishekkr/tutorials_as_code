
## Chapter.12 Control Flow

> code example at [chapter-12.exs](./chapter-12.exs)

* try be declarative not imperative; combo of guard clauses and pattern-match replaces most of control flow required elsewhere

* before using these consider functional alternative; will be shorter & focused


### if and unless

* `else` branch is optional

```
if <condition>, do: this(), else: that()
if <condition>, do: this()
unless <condition>, do: that(), else: this()
unless <condition>, do: that()
```


### cond

* this macro executes code corresponding to most truthy condition

```
cond do
  some_condition() ->
    dox()
  other_condition() ->
    doy()
  true ->
    doz()
end
```


### case

* lets use set of patterns, executes code with first match & return output

```
DATA = %{a: 1, b: 2}
case DATA do
  d = %{b: 2} when rem(b, 2) == 0 and rem(b, 3) != 0 ->
    d.a
  _ -> 0
end
```


### Raising Exceptions

* not control flow structures; intended for disasters that bring down service

> * db/dns going down could be exceptional; better to build retry with back-off
>
> * failure to open config file; seems erroneous especially if during initiation
>
> * failing to open a file, is just a different return-value flow to be followed

* at simplest `raise "msg"` raises RuntimeError

> can specify as `raise SomeError, message: "what"`

* should propagate back up to external, supervising process (**covered later**)

* can catch exceptions, but shouldn't be the first choice (**covered later**)


### Designing with Exceptions

* could let elixir raise an exception by using pattern-match and describe only cases you want to handle; although it wouldn't be informative

* some functions have `!` variant, like `File.open!` to raise an exception on error instead of bubbling up `{:ok|:error, RESULT}`


### Doing more with less

> generally less control flows doesn't effect, the way Elixir code is represented

---
