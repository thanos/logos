defmodule Logos.Data do
  @moduledoc """
  Deterministic JSON for event payloads. Object keys are sorted.
  """

  def decode(text) when is_binary(text) do
    case String.trim(text) do
      empty when empty in ["{}", "%{}"] ->
        %{}

      "{" <> _ = object ->
        decode_object(object)

      other ->
        other
    end
  end

  def encode(value), do: encode(value, "")

  defp encode(map, "") when is_map(map) do
    inner =
      map
      |> Enum.sort_by(fn {key, _} -> to_string(key) end)
      |> Enum.map(fn {key, item} -> encode(to_string(key), "") <> ":" <> encode(item, "") end)
      |> Enum.join(",")

    "{" <> inner <> "}"
  end

  defp encode(list, "") when is_list(list) do
    "[" <> Enum.map_join(list, ",", &encode(&1, "")) <> "]"
  end

  defp encode(value, "") when is_binary(value), do: "\"" <> escape(value) <> "\""
  defp encode(value, "") when is_integer(value), do: Integer.to_string(value)
  defp encode(true, ""), do: "true"
  defp encode(false, ""), do: "false"
  defp encode(nil, ""), do: "null"
  defp encode(:none, ""), do: "null"
  defp encode({:some, value}, ""), do: encode(value, "")
  defp encode({:raw, value}, ""), do: encode(to_string(value), "")
  defp encode({:decimal, coef, scale}, ""), do: format_decimal(coef, scale)

  def format_decimal(coef, scale) when is_integer(coef) and is_integer(scale) and scale >= 0 do
    sign = if coef < 0, do: "-", else: ""
    digits = coef |> abs() |> Integer.to_string() |> String.pad_leading(scale + 1, "0")
    {whole, frac} = String.split_at(digits, String.length(digits) - scale)
    if scale == 0, do: sign <> whole, else: sign <> whole <> "." <> frac
  end

  defp decode_object(text) do
    body = text |> String.trim() |> String.trim_leading("{") |> String.trim_trailing("}")

    body
    |> split_top_level(",")
    |> Map.new(fn pair ->
      [key, value] = String.split(pair, ":", parts: 2)
      {trim_quotes(key), decode_literal(String.trim(value))}
    end)
  end

  defp decode_literal("true"), do: true
  defp decode_literal("false"), do: false
  defp decode_literal("null"), do: nil

  defp decode_literal(value) do
    cond do
      String.match?(value, ~r/^-?\d+$/) ->
        String.to_integer(value)

      String.match?(value, ~r/^-?\d+\.\d+$/) ->
        decimal(value)

      String.starts_with?(value, "\"") ->
        trim_quotes(value)

      true ->
        {:raw, value}
    end
  end

  def decimal(value) when is_binary(value) do
    [whole, frac] = String.split(value, ".")
    {:decimal, String.to_integer(whole <> frac), String.length(frac)}
  end

  defp trim_quotes(value) do
    value
    |> String.trim()
    |> String.trim_leading("\"")
    |> String.trim_trailing("\"")
  end

  defp escape(value) do
    value
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
  end

  defp split_top_level(text, delim) do
    {parts, buf, _lvl, _in_q} =
      text
      |> String.codepoints()
      |> Enum.reduce({[], "", 0, false}, fn ch, {acc, cur, lvl, q} ->
        cond do
          ch == "\"" -> {acc, cur <> ch, lvl, !q}
          q -> {acc, cur <> ch, lvl, q}
          ch in ["{", "["] -> {acc, cur <> ch, lvl + 1, q}
          ch in ["}", "]"] -> {acc, cur <> ch, lvl - 1, q}
          ch == delim and lvl == 0 -> {[String.trim(cur) | acc], "", lvl, q}
          true -> {acc, cur <> ch, lvl, q}
        end
      end)

    [buf | parts] |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == "")) |> Enum.reverse()
  end
end
