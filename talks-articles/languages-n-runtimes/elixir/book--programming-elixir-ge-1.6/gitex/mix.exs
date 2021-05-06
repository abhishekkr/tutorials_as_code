defmodule Gitex.MixProject do
  use Mix.Project

  def project do
    [
      app: :gitex,
      escript: escript_config(),
      version: "0.1.0",
      elixir: "~> 1.9",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      ## ex_doc config
      name: "Gitex",
      source_url: "https://github.com/abhishekkr/tutorials_as_code",
      homepage_url: "https://github.com/abhishekkr/tutorials_as_code",
      docs: [
        main: "Gitex", ## main page in docs
        logo: "./gitex.png",
        extras: ["README.md"]
      ],

      ## excoveralls config
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:httpoison, "~> 1.8"}, ## http client
      {:poison, "~> 4.0"},    ## json
      {:stream_data, "~> 0.5.0", only: :test}, ## property testing
      {:mock, "~> 0.3", only: :test},    ## test mocking
      {:excoveralls, "~> 0.14.0", only: :test},  ## test coverage
      {:ex_doc, "~> 0.24.2", only: :dev, runtime: false},  ## load only in dev mode
      {:earmark, "~> 1.4", only: :dev, runtime: false} ## doc output formatter
    ]
  end

  defp escript_config do
    [main_module: Gitex.CLI]
  end
end
