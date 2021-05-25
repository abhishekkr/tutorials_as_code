defmodule Ehlo.Repo do
  use Ecto.Repo,
    otp_app: :ehlo,
    adapter: Ecto.Adapters.Postgres
end
