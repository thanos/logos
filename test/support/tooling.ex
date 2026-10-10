defmodule Logos.Conformance.Tooling do
  @moduledoc """
  Detects pinned external tools. Absence is reported as `NOT_RUN`, never as a pass.
  """

  @solc_version "0.8.28"
  @anvil_hardfork "shanghai"

  def solc_version, do: @solc_version
  def anvil_hardfork, do: @anvil_hardfork

  def anvil_path, do: System.find_executable("anvil")
  def solc_path, do: System.find_executable("solc")
  def cast_path, do: System.find_executable("cast")

  def anvil?, do: anvil_path() != nil
  def solc?, do: solc_path() != nil

  def versions do
    %{
      "elixir" => System.version(),
      "otp" => System.otp_release(),
      "anvil" => version_of("anvil", ["--version"]),
      "solc" => version_of("solc", ["--version"]),
      "cast" => version_of("cast", ["--version"])
    }
  end

  defp version_of(cmd, args) do
    case System.find_executable(cmd) do
      nil ->
        "NOT_INSTALLED"

      path ->
        case System.cmd(path, args, stderr_to_stdout: true) do
          {out, 0} -> out |> String.trim() |> String.split("\n") |> List.first()
          {out, _} -> "ERROR " <> String.trim(out)
        end
    end
  end
end
