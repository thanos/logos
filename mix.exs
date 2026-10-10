defmodule Logos.MixProject do
  use Mix.Project

  def project do
    [
      app: :logos,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        "test.watch": :test,
        coveralls: :test,
        "coveralls.html": :test,
        "coveralls.detail": :test,
        "coveralls.json": :test,
        "coveralls.xml": :test
      ],
      aliases: [
        "test.watch": ["test --listen-on-stdin"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application, do: [extra_applications: [:logger, :crypto]]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.34", only: :dev, runtime: false},
      {:excoveralls, "~> 0.18", only: :test, runtime: false},
      {:stream_data, "~> 1.1", only: :test, runtime: false}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      maintainers: ["Thanos Vassilakis"],
      licenses: ["MPL-2.0"],
      links: %{
        "GitHub" => "https://github.com/thanos/logos",
        "Documentation" => "https://hexdocs.pm/logos"
      }
    ]
  end
end
