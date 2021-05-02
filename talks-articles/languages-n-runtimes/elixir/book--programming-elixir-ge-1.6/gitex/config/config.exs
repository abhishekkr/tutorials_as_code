use Mix.Config

config :gitex, github_api: "https://api.github.com"

config :logger, compile_time_purge_level: :info

import_config "#{Mix.env}.ex"
