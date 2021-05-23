## example of inspect
defprotocol Xnspect do
  @fallback_to_any true

  @spec inspect(t, Inspect.Opts.t()) :: Inspect.Algebra.t()
  def inspect(thing, opts \\ nil)
end

defimpl Xnspect, for: PID do
  def inspect(pid, _opts) do
    "#PID" <> IO.iodata_to_binary(:erlang.pid_to_list(pid)) <> "!$"
  end
end

defimpl Xnspect, for: Reference do
  def inspect(ref, _opts) do
    '#Ref' ++ rest = :erlang.ref_to_list(ref)
    "#Reference" <> IO.iodata_to_binary(rest)
  end
end

pid = spawn(IO, :puts, ["oye"])
Xnspect.inspect pid |> IO.inspect


defprotocol Collection do
  @fallback_to_any true

  def is?(value)
end

defimpl Collection, for: [List, Tuple, BitString, Map] do
  def is?(_), do: true
end

defimpl Collection, for: Any do
  def is?(_), do: false
end

Enum.each(
  [:what, 1, 1.0, 'abc', "abc", [1,2], {2,3}, %{a: 1}, <<0>>],
  fn v -> IO.puts("#{inspect v}: #{Collection.is?(v)}") end
)


defmodule Blob do
  defstruct content: nil
end

defmodule TBlob do
  def run do
    b = %Blob{content: 101}
    IO.inspect(b)
    IO.inspect(b, structs: false)
  end
end

TBlob.run
