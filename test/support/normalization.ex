defmodule Logos.Conformance.Normalization do
  @moduledoc """
  Canonical JSON values for corpus comparison.

  Integers are decimal strings so uint256 is lossless. Hex strings are
  lowercase. Address-sized hex is compared case-insensitively by the same
  rule. Diagnostic messages keep line and column text and drop absolute paths.
  """

  @address_hex ~r/^0x[0-9a-fA-F]{40}$/

  def canonicalize(value), do: canon(value)

  def encode(value) do
    value
    |> canonicalize()
    |> encode_sorted()
  end

  def ast(%Logos.AST.Contract{} = contract) do
    %{
      "name" => contract.name,
      "parties" => contract.parties,
      "defs" => contract.defs,
      "state" => contract.state,
      "clauses" => Enum.map(contract.clauses, &clause/1),
      "term" => contract.term,
      "law" => contract.law,
      "signatures" => contract.signatures
    }
    |> canonicalize()
  end

  def exception(module, message) when is_atom(module) and is_binary(message) do
    %{
      "status" => "error",
      "exception" => module |> Module.split() |> List.last(),
      "message" => scrub_message(message)
    }
  end

  def scrub_message(message) do
    Regex.replace(~r/(?:\/[\w.\-]+)+\/([\w.\-]+)/, message, "\\1")
  end

  defp clause(%Logos.AST.Clause{} = clause) do
    %{
      "name" => clause.name,
      "params" =>
        Enum.map(clause.params, fn %{name: name, type: type} ->
          %{"name" => name, "type" => Atom.to_string(type)}
        end),
      "provided" => clause.provided,
      "shall" => effect(clause.shall),
      "remedies" => if(clause.remedies, do: effect(clause.remedies), else: nil)
    }
  end

  defp effect(%Logos.AST.Eff{op: op, args: args}) do
    %{"op" => Atom.to_string(op), "args" => normalize_args(args)}
  end

  defp normalize_args(effects) when is_list(effects), do: Enum.map(effects, &effect/1)
  defp normalize_args(args), do: args

  defp canon(nil), do: nil
  defp canon(value) when is_boolean(value), do: value
  defp canon(value) when is_integer(value), do: Integer.to_string(value)
  defp canon(value) when is_float(value), do: raise("floats are not a canonical corpus value")
  defp canon(value) when is_atom(value), do: Atom.to_string(value)
  defp canon(value) when is_binary(value), do: canon_string(value)
  defp canon(value) when is_list(value), do: Enum.map(value, &canon/1)
  defp canon({:raw, value}), do: %{"raw" => canon(value)}
  defp canon({:some, value}), do: %{"some" => canon(value)}

  defp canon(%_{} = struct) do
    raise "refusing to canonicalize struct #{inspect(struct.__struct__)}"
  end

  defp canon(value) when is_map(value) do
    Map.new(value, fn {key, item} -> {canon_key(key), canon(item)} end)
  end

  defp canon(value) when is_tuple(value), do: raise("unsupported tuple #{inspect(value)}")
  defp canon(value), do: raise("unsupported corpus value #{inspect(value)}")

  defp canon_key(key) when is_atom(key), do: Atom.to_string(key)
  defp canon_key(key) when is_binary(key), do: key

  defp canon_string("0x" <> rest = value) do
    if String.match?(rest, ~r/^[0-9a-fA-F]+$/) do
      "0x" <> String.downcase(rest)
    else
      value
    end
  end

  defp canon_string(value) do
    if String.match?(value, @address_hex) do
      String.downcase(value)
    else
      scrub_message(value)
    end
  end

  defp encode_sorted(value) when is_map(value) do
    pairs =
      value
      |> Enum.sort_by(fn {key, _} -> key end)
      |> Enum.map(fn {key, item} -> [JSON.encode!(key), ":", encode_sorted(item)] end)
      |> Enum.intersperse(",")

    ["{", pairs, "}"] |> IO.iodata_to_binary()
  end

  defp encode_sorted(value) when is_list(value) do
    ["[", Enum.map(value, &encode_sorted/1) |> Enum.intersperse(","), "]"]
    |> IO.iodata_to_binary()
  end

  defp encode_sorted(value) when is_binary(value) or is_boolean(value) or is_nil(value) do
    JSON.encode!(value)
  end
end
