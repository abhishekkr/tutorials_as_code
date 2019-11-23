
## error handling
#
#### * convention of Elixir function `foo(...)` returning `{:ok, result}` and `{:error, reason}`;
####   a separate function `foo!(...)`returning `result` or raise an error

#### * raise an error using `raise "Reason"` or `raise <ErrorType>, message: "reason"`

#### * can handle raised error using try/rescue and pattern matching
try do
  raise "Say booo!"
rescue
  e in RuntimeError -> IO.puts("An error: " <> e.message)
end

try do
  [] |> Keyword.fetch!(:blah_missing) |> File.read!()
rescue ## matching multiple errors in single rescue
  _e in KeyError -> IO.puts("missing :blah_missing")
  e in File.Error -> IO.puts("error reading file, " <> e.message)
  e in RuntimeError -> IO.puts("An error: " <> e.message)
end


### after
#
#### * for cases to ensure some action after try/rescue irrespective of error being raised

try do
  IO.puts "Say hey!"
rescue
  e in RuntimeError -> IO.puts("An error: " <> e.message)
after
  IO.puts "Cleanup leftovers."
end

try do
  raise "Say booo!"
rescue
  e in RuntimeError -> IO.puts("An error: " <> e.message)
after
  IO.puts "Cleanup leftovers."
end


### new errors
#
#### * when more specific than built-in error types like `RuntimeError` are needed, `defexception/1` macro can be used

defmodule UserNotFoundError do
  defexception message: "an example error has occured, to be raised when no user was found during an activity"
end

try do
  raise UserNotFoundError
rescue
  e in UserNotFoundError -> IO.puts(e.message)
end


### throws
#
#### * `try/throw/catch` is anoter practice, as stopgaps when lib fail to provide adequate APIs
try do
  for some_var <- 1..5 do
    if some_var > 3, do: throw(some_var)
    IO.puts(some_var)
  end
catch
  some_var -> IO.puts("Caught: #{some_var}")
end


### exiting
#
#### * whenever a process dies during critical fault tolerance part of code
#
####   explicitly we can use, `spawn_link fn -> exit("try again") end`
#
#### * it's possible to catch an exit, should be used rarely when truly needed
#

try do
  exit "try again"
catch
  :exit, reason -> IO.puts reason
end

##################################################
