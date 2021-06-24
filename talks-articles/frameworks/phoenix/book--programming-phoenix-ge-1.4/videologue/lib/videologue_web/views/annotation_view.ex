defmodule VideologueWeb.AnnotationView do
  use VideologueWeb, :view

  def render("annotation.json", %{annotation: annotation}) do
    %{
      id: annotation.id,
      body: annotation.body,
      at: annotation.at,
      user: render_one(annotation.user, VideologueWeb.UserView, "user.json")
    }
  end
end
