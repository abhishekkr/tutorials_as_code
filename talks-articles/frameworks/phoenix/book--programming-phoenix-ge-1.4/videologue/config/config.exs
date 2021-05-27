# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :videologue,
  ecto_repos: [Videologue.Repo]

# Configures the endpoint
config :videologue, VideologueWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "8tqaSDjP/UkrNQBol2jLr/duNngQ19YbZXe+7TcVhKNa+oPQKY76HbY9tSv1TBU8",
  render_errors: [view: VideologueWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Videologue.PubSub,
  live_view: [signing_salt: "F8FUwWqa"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
