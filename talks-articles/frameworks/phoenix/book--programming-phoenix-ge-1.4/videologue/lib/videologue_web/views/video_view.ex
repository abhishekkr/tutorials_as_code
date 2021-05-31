defmodule VideologueWeb.VideoView do
  use VideologueWeb, :view

  def category_select_options(categories) do
    categories |> Enum.map(fn c -> {c.name, c.id} end)
  end
end
