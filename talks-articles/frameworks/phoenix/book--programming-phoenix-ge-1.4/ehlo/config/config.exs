# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :ehlo,
  ecto_repos: [Ehlo.Repo]

# Configures the endpoint
config :ehlo, EhloWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "HGEZI5Bdl3nArtm/PPq9rW3N11/He7k5MY9hsbJK+FMTUFen2S44JiNxen1Jbl0d",
  render_errors: [view: EhloWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Ehlo.PubSub,
  live_view: [signing_salt: "OnD+jI9E"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
