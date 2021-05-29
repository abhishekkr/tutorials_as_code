defmodule Videologue.Repo.Migrations.CreateUsersFieldPasswordHash do
  use Ecto.Migration

  def change do
    default_hash = Pbkdf2.hash_pwd_salt("changeit")
    Videologue.Repo.update_all("users", set: [password_hash: default_hash])
  end
end
