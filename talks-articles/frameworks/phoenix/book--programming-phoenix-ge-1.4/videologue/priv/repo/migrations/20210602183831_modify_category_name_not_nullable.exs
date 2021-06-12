defmodule Videologue.Repo.Migrations.ModifyCategoryNameNotNullable do
  use Ecto.Migration

  def change do
    alter table(:categories) do
      modify(:name, :string, null: false, from: :string) # Title column is now not nullable
    end
  end
end
