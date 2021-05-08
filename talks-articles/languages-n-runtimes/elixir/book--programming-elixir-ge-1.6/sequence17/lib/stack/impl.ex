defmodule Stack.Impl do
  def pop([]), do: {nil, []}
  def pop([h|tail]), do: {h, tail}

  def push(elem, list), do: [elem|list]
end
