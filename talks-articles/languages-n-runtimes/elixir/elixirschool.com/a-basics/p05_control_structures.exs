## control structures


### if and unless
#### * if/2 and unless/2 are defined as macros in 'Kernel', not a language construct
if String.valid?(100) do
  "valid string"
else
  "invalid string"
end |> IO.inspect()

if 100 do
  "Truthy"
end |> IO.inspect()

unless nil do
  "nil as false"
end |> IO.inspect()


### case
#### * case/2 to match against multiple patterns, shall have a match-all case at end to avoid it throw exception
case {:ok, "ehlo"} do
  {:ok, result} -> result
  {:error} -> "what"
  _ -> "match all"
end |> IO.inspect()
#### * to match against existing variables utilize pin operator
khokha = :lac
case :lac do
  ^khokha -> "kasa"
  khokha -> "kai"
end |> IO.inspect()
case :blac do
  ^khokha -> "kasa"
  khokha -> "kai"
end |> IO.inspect()
#### * supports guard clauses
case {:error, 401} do
  {:ok, result} -> result
  {:error, code} when is_number(code) and code >= 500 -> "internal error"
  {:error, code} -> "client error"
  _ -> "unhandled yet, manage it"
end |> IO.inspect()


### cond
#### * cond/1 used when matching conditions is important than values
cx = "qwertyuiopasdfghjkl"
cond do
  String.length(cx) <= 0 -> "empty"
  String.length(cx) <= 10 -> "string"
  String.length(cx) <= 100 -> "big string"
  true -> "real big string"
end |> IO.inspect()


### with
#### * with/1 useful where nested case/2 statement can't cleanly be piped
#### * composed of keywords, generators, finally expression
w_user = %{fname: "Sean", lname: "Connery"}
with {:ok, fname} <- Map.fetch(w_user, :fname),
     {:ok, lname} <- Map.fetch(w_user, :lname),
     do: IO.inspect(lname <> ", " <> fname)
with {:ok, fname} <- Map.fetch(w_user, :fname),
     {:ok, sname} <- Map.fetch(w_user, :sname),
     do: IO.inspect(sname <> ", " <> fname)
#### * nested case/2 like flow in linear with/1
with {:ok, code, response} <- {:ok, 200, "success"},
     {:ok, to_render} <- {:ok, "#{code} :: #{response}"} do
  IO.puts(to_render)
end
#### * also supports 'else' flow
with {:ok, code, response} <- {:ok, 200, "success"},
     {:ok, to_render} <- {:error, nil} do
  IO.puts(to_render)
else
  {:error, nil} -> IO.puts("there has been an issue")
  _ -> IO.puts("something is odd")
end
