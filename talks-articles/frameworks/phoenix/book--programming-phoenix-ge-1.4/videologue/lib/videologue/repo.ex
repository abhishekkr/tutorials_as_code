defmodule Videologue.Repo do
  use Ecto.Repo,
    otp_app: :videologue,
    adapter: Ecto.Adapters.Postgres
end
