use Bitwise

defmodule Find do
  def missing_item(superset, subset), do: Bitwise.bxor(list_xor(superset), list_xor(subset))

  def ascii_to_str(num), do: List.to_string([num])

  def list_xor(lst), do: do_list_xor(lst, 0)
  defp do_list_xor([], xor), do: xor
  defp do_list_xor([head | tail], xor), do: do_list_xor(tail, Bitwise.bxor(head, xor))
end

## main
Find.missing_item([1,3,5,7], [1,3,7]) |> IO.puts
Find.missing_item([1,3,5,7,9], [1,3,7,9]) |> IO.puts
Find.missing_item([1,3,3,7,9], [1,3,7,9]) |> IO.puts
Find.missing_item('abcde', 'abde') |> Find.ascii_to_str |> IO.puts

