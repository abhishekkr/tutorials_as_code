defmodule TomlParseError do
  defexception message: "TOML Parsing Error",
              can_retry: false

  def full_message(me) do
    "TOML parse filed: #{me.message}, retriable: #{me.can_retry}"
  end
end

defmodule Toml do
  def parse do
    raise TomlParseError, message: "bad entry"
  end

  def schedule_retry do
    :ok
  end
end


try do
  Toml.parse()
rescue
  error in [TomlParseError] ->
    TomlParseError.full_message(error) |> IO.puts()
    if error.can_retry, do: Toml.schedule_retry()
end
