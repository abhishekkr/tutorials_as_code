
## Appendix.1 Exceptions: raise and trym catch and throw

> code samples at [chapter-26-appendix-1](./chapter-26-appendix-1)
>
> Elixir, like Erlang believes ERRORS should be FATAL. Effects will be localized, Supervisor will detect the failure and restart it.
>
> Exceptions are for exceptional things, rarely to be caught.

### Raising an Exception

* simply `raise "Boom"` generates a RuntimeError exception

> can also pass type and a message to `raise` as `raise FunctionClauseError, message: "call me by correct id"`

* exceptions can be intercepted via `try` block

> `try` with optional clauses
>
> * `rescue` or `catch`: taking pattern and code to run
>
> * `after`: always run at end of `try` regardless of exception occurance

> code example [try-catch-rescue-after.exs](chapter-26-appendix-1/try-catch-rescue-after.exs)


### catch, exit and throw

* can raise a second kind error, generated when a process calls `error`, `exit` or `throw`

> code example at [try-catch-exit-throw.exs](chapter-26-appendix-1/try-catch-exit-throw.exs)


### Defining your own Exceptions

* exceptions are records defined by `defexception` for various fields in exception

* required field `message`, additional `can_retry`

> code example at [toml-parse-error.exs](./chapter-26-appendix-1/toml-parse-error.exs)


### Now ignore this Appendix

* Elixir Compiler contains less than 10 exception handlers.

* MIX utility got no exception handlers.

> Try isolating code in separate process instead. If it can go wrong, better isolated and restarted.

---
