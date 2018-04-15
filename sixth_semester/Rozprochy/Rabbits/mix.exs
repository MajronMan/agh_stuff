defmodule Rabbits.Mixfile do
  use Mix.Project

  def project do
    [
      app: :rabbits,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      applications: [:amqp],
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:amqp, "~> 0.2.3"},
      {:rabbit_common, "3.6.15"},
      {:recon, "~> 2.3.2"},
      {:apex, "~>1.2.0"}
    ]
  end
end
