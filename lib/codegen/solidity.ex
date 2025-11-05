defmodule Logos.Codegen.Solidity do
  @moduledoc false
  alias Logos.AST.{Contract, Clause, Eff}

  def emit(%Contract{} = c) do
    [
      "pragma solidity ^0.8.20;\n",
      header(c),
      "contract ",
      c.name,
      " {\n",
      "  // ---- parties ----\n",
      for {id, addr} <- c.parties do
        "  address public " <> id <> " = " <> solidity_addr(addr) <> ";\n"
      end,
      "\n  // ---- state ----\n",
      for {k, v} <- c.state do
        "  " <> storage_decl(k, v) <> " = " <> lit(v) <> ";\n"
      end,
      "\n  // ---- events ----\n",
      "  event Emitted(string ev, string json);\n",
      "  event TransferNote(string asset, address from_, address to_, uint256 amount);\n",
      "\n  // ---- clauses ----\n",
      for %Clause{} = cl <- c.clauses do
        emit_clause(c, cl)
      end,
      "}\n"
    ]
    |> IO.iodata_to_binary()
  end

  defp header(c) do
    law = if c.law, do: " // Law: " <> c.law <> "\n", else: "\n"
    ["// Auto-generated from legal DSL\n", law]
  end

  defp storage_decl(k, v) do
    type =
      case v do
        true -> "bool"
        false -> "bool"
        i when is_integer(i) -> "int256"
        # default; customize as needed
        {:raw, _} -> "int256"
        # represent Option None as false sentinel
        :none -> "bool"
        {:some, _} -> "bool"
        <<"0x", _::binary>> -> "address"
        _ -> "string"
      end

    type <> " public " <> k
  end

  defp solidity_addr(<<"0x", _::binary>> = a), do: a
  defp solidity_addr(other), do: "address(uint160(uint(keccak256(bytes(" <> lit(other) <> ")))))"

  # ---- clause emit ----------------------------------------------------------

  defp emit_clause(contract, %Clause{
         name: name,
         params: params,
         provided: prov,
         shall: shall,
         remedies: rem
       }) do
    sig =
      params
      # simple typed params; adjust as needed
      |> Enum.map(fn p -> "int256 " <> p end)
      |> Enum.join(", ")

    reqs =
      prov
      |> Enum.map(&compile_pred(contract, &1))
      |> Enum.map(fn pred -> "    require(" <> pred <> ", \"ProvidedThat failed\");\n" end)
      |> case do
        [] -> ""
        xs -> Enum.join(xs)
      end

    body =
      if rem do
        # We model Provided as a combined predicate (all-and); if fails -> remedies
        pred_all =
          case prov do
            [] ->
              "true"

            _ ->
              prov
              |> Enum.map(&compile_pred(contract, &1))
              |> Enum.join(" && ")
          end

        [
          "  function ",
          name,
          "(",
          sig,
          ") external {\n",
          "    if (!(",
          pred_all,
          ")) {\n",
          indent(compile_effect(contract, rem), 6),
          "      return;\n",
          "    }\n",
          indent(compile_effect(contract, shall), 4),
          "  }\n\n"
        ]
      else
        [
          "  function ",
          name,
          "(",
          sig,
          ") external {\n",
          reqs,
          indent(compile_effect(contract, shall), 4),
          "  }\n\n"
        ]
      end

    IO.iodata_to_binary(body)
  end

  # ---- predicates (very small subset) --------------------------------------

  defp compile_pred(_c, p) do
    p
    |> String.replace("=", "==")
    |> String.replace("<>", "!=")
    |> String.replace(~r/\bnot\b/, "!")
    |> String.replace(~r/\band\b/, "&&")
    |> String.replace(~r/\bor\b/, "||")
    |> String.replace(~r/\btrue\b/, "true")
    |> String.replace(~r/\bfalse\b/, "false")
  end

  # ---- effects --------------------------------------------------------------

  defp compile_effect(_c, %Eff{op: :noop}), do: "    // NoOp\n"

  defp compile_effect(c, %Eff{op: :all, args: list}) do
    list
    |> Enum.map(&compile_effect(c, &1))
    |> Enum.join()
  end

  defp compile_effect(_c, %Eff{op: :emit, args: %{"event" => ev} = m}) do
    data =
      case Map.get(m, "data") do
        nil -> "{}"
        s when is_binary(s) -> s
        other -> inspect(other)
      end

    "    emit Emitted(" <> lit(ev) <> ", " <> lit(data) <> ");\n"
  end

  defp compile_effect(_c, %Eff{op: :set, args: %{"field" => f, "value" => v}}) do
    "    " <> f <> " = " <> lit(v) <> ";\n"
  end

  defp compile_effect(c, %Eff{op: :if, args: %{"cond" => cond, "then" => t, "else" => e}}) do
    [
      "    if (",
      compile_pred(c, cond),
      ") {\n",
      indent(compile_effect(c, t), 6),
      "    } else {\n",
      indent(compile_effect(c, e), 6),
      "    }\n"
    ]
    |> IO.iodata_to_binary()
  end

  # Note: Prototype emits an event. To wire an actual ERC-20 call, map asset to address & call IERC20.
  defp compile_effect(_c, %Eff{op: :transfer, args: kv}) do
    asset = Map.get(kv, "asset", "\"asset\"") |> to_asset_string()
    from_ = Map.get(kv, "from", "address(0)") |> to_address_expr()
    to_ = Map.get(kv, "to", "address(0)") |> to_address_expr()
    amount = Map.get(kv, "amount", "0") |> to_uint_expr()

    "    emit TransferNote(" <>
      lit(asset) <> ", " <> from_ <> ", " <> to_ <> ", " <> amount <> ");\n"
  end

  defp to_asset_string(s) when is_binary(s), do: s
  defp to_asset_string({:raw, s}), do: s
  defp to_asset_string(other), do: inspect(other)

  defp to_address_expr(<<"0x", _::binary>> = a), do: a
  defp to_address_expr(id) when is_binary(id), do: id
  defp to_address_expr({:raw, id}), do: id
  defp to_address_expr(_), do: "address(0)"

  defp to_uint_expr(i) when is_integer(i), do: Integer.to_string(i)
  defp to_uint_expr({:raw, s}), do: s
  defp to_uint_expr(s) when is_binary(s), do: s
  defp to_uint_expr(_), do: "0"

  # ---- helpers --------------------------------------------------------------

  defp indent(s, n) do
    pad = String.duplicate(" ", n)

    s
    |> String.split("\n", trim: false)
    |> Enum.map_join("\n", fn
      "" -> ""
      line -> pad <> line
    end)
    |> String.trim_leading()
  end

  defp lit(true), do: "true"
  defp lit(false), do: "false"
  defp lit(i) when is_integer(i), do: Integer.to_string(i)
  defp lit(<<"0x", _::binary>> = a), do: a
  defp lit({:raw, s}), do: s
  # sentinel; extend to real Option encoding
  defp lit({:some, _v}), do: "true"
  defp lit(:none), do: "false"

  defp lit(s) when is_binary(s) do
    "\"" <> String.replace(s, "\"", "\\\"") <> "\""
  end

  defp lit(other), do: "\"" <> inspect(other) <> "\""
end
