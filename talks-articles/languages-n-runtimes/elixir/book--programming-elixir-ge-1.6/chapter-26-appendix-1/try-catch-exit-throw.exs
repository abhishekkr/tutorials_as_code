defmodule Thy do
  def rebar(n) do
    try do
      incite(n)
    catch
      :exit, code -> "Exit with code #{inspect code}"
      :throw, val -> "Throw with value #{inspect val}"
      what, val -> "#{inspect what} with #{inspect val}"
    end
  end

  defp incite(1), do: exit(:something_bad_happened)
  defp incite(2), do: throw {:animal, "wombat"}
  defp incite(3), do: :erlang.error "Bamm!"
end

Thy.rebar(1) |> IO.inspect()
Thy.rebar(2) |> IO.inspect()
Thy.rebar(3) |> IO.inspect()
