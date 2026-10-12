defmodule Logos.Duration do
  @moduledoc """
  Converts `Duration("1d12h")` and bare `7d` / `1d12h` into seconds.
  """

  @units %{"s" => 1, "m" => 60, "h" => 3_600, "d" => 86_400, "w" => 604_800}

  @doc "Sum every `<number><unit>` pair in `spec`."
  def seconds(spec) when is_binary(spec) do
    case Regex.scan(~r/(\d+)\s*([smhdw])/i, spec) do
      [] ->
        raise "bad duration #{inspect(spec)}"

      parts ->
        Enum.reduce(parts, 0, fn [_, n, u], acc -> acc + unit_seconds(n, u) end)
    end
  end

  @doc "Replace duration literals with decimal seconds. Leaves `now` alone."
  def rewrite(text) when is_binary(text) do
    text
    |> then(&Regex.replace(~r/Duration\(\s*["']([^"']+)["']\s*\)/i, &1, fn _, spec ->
      Integer.to_string(seconds(spec))
    end))
    |> then(&Regex.replace(~r/\b(?:\d+\s*[smhdw]){1,}\b/i, &1, fn spec ->
      Integer.to_string(seconds(spec))
    end))
  end

  @doc "Solidity form: `now` is `block.timestamp` and durations are seconds."
  def rewrite_solidity(text) when is_binary(text) do
    text
    |> String.replace(~r/\bnow\b/, "block.timestamp")
    |> rewrite()
  end

  defp unit_seconds(n, u) do
    {num, _} = Integer.parse(n)
    Map.fetch!(@units, String.downcase(u)) * num
  end
end
