defmodule Videologue.Multimedia.Category do
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  schema "categories" do
    field :name, :string, null: false

    timestamps()
  end

  @doc false
  def changeset(category, attrs \\ %{}) do
    category
    |> cast(attrs, [:name])
    |> validate_required([:name])
    |> validate_length(:name, min: 1)
    |> unique_constraint(:name)
  end

  @doc false
  def alphabetical(query) do
    from c in query, order_by: c.name
  end
end
