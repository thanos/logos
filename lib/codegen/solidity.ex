defmodule Logos.Codegen.Solidity do
  @moduledoc """
  Backend that emits **Solidity** source from the DSL AST.

  It’s intentionally small/transparent for inspection and round-tripping.
  For on-chain deployment, use the EVM bytecode backend.
  """

  alias Logos.AST.{Contract, Clause, Eff}

  @doc "Emit Solidity source for a parsed contract."
  @spec emit(Contract.t()) :: String.t()
  def emit(%Contract{} = c) do
    iodata = [
      "pragma solidity ^0.8.20;\n",
      header(c),
      "contract ",
      c.name,
      " {\n",
      "  // ---- parties ----\n",
      for {id, addr} <- c.parties do
        ["  address public ", id, " = ", solidity_addr(addr), ";\n"]
      end,
      "\n  // ---- state ----\n",
      for {k, v} <- c.state do
        ["  ", storage_decl(k, v), " = ", lit(v), ";\n"]
      end,
      "\n  // ---- events ----\n",
      "  event Emitted(string ev, string json);\n",
      "  event TransferNote(address asset, address from_, address to_, uint256 amount);\n",
      "\n  // ---- clauses ----\n",
      for %Clause{} = cl <- c.clauses do
        emit_clause(c, cl)
      end,
      "}\n"
    ]

    IO.iodata_to_binary(iodata)
  end

  # ---------------------------------------------------------------------------

  defp header(%Contract{law: nil}), do: "// Auto-generated from legal DSL\n\n"

  defp header(%Contract{law: law}),
    do: ["// Auto-generated from legal DSL\n// Governing law: ", law, "\n\n"]

  defp storage_decl(key, value) do
    sol_type =
      cond do
        is_boolean(value) ->
          "bool"

        is_integer(value) ->
          "int256"

        is_binary(value) and String.starts_with?(value, "0x") and byte_size(value) >= 42 ->
          "address"

        match?({:raw, _}, value) ->
          "int256"

        value in [:none] or match?({:some, _}, value) ->
          "bool"

        true ->
          "string"
      end

    sol_type <> " public " <> key
  end

  defp solidity_addr(<<"0x", _::binary>> = a), do: a

  # Derive a pseudo-address from a string literal (rare path, for convenience)
  defp solidity_addr(other) when is_binary(other) do
    # address(uint160(uint(keccak256(bytes("<other>")))))
    ["address(uint160(uint(keccak256(bytes(", lit(other), ")))))"] |> IO.iodata_to_binary()
  end

  # ----------------------------- clauses -------------------------------------

  defp emit_clause(contract, %Clause{
         name: name,
         params: params,
         provided: prov,
         shall: shall,
         remedies: rem
       }) do
    # For readability we render all params as int256/address/bool based on declared type.
    sig =
      params
      |> Enum.map(fn %{name: n, type: t} -> [sol_param_type(t), " ", n] end)
      |> Enum.intersperse(", ")
      |> IO.iodata_to_binary()

    # Remedies branch: if provided fails, execute remedies and return; else do shall.
    cond do
      rem != nil ->
        pred_all =
          case prov do
            [] -> "true"
            _ -> prov |> Enum.map(&compile_pred(contract, &1)) |> Enum.join(" && ")
          end

        body = [
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

        IO.iodata_to_binary(body)

      true ->
        reqs =
          prov
          |> Enum.map(&compile_pred(contract, &1))
          |> Enum.map(fn p -> ["    require(", p, ', "ProvidedThat failed");', ?\n] end)

        body = [
          "  function ",
          name,
          "(",
          sig,
          ") external {\n",
          reqs,
          indent(compile_effect(contract, shall), 4),
          "  }\n\n"
        ]

        IO.iodata_to_binary(body)
    end
  end

  defp sol_param_type(:address), do: "address"
  defp sol_param_type(:bool), do: "bool"
  defp sol_param_type(:uint256), do: "uint256"
  defp sol_param_type(:string), do: "string"
  defp sol_param_type(:bytes), do: "bytes"
  defp sol_param_type(_), do: "uint256"

  # ------------------------- predicates & effects -----------------------------

  # Very light rewrite: AND/OR/NOT to Solidity, keep existing operators.
  defp compile_pred(_c, p) when is_binary(p) do
    p
    |> Logos.Duration.rewrite_solidity()
    |> replace_word("and", "&&")
    |> replace_word("or", "||")
    |> replace_word("not", "!")

    # keep ==, !=, <, >, <=, >= intact; do NOT touch single '=' anywhere
  end

  defp replace_word(text, word, repl) do
    Regex.replace(~r/\b#{Regex.escape(word)}\b/, text, repl)
  end

  defp compile_effect(_c, %Eff{op: :noop}), do: "    // NoOp\n"

  defp compile_effect(c, %Eff{op: :all, args: list}) when is_list(list) do
    list
    |> Enum.map(&compile_effect(c, &1))
    |> Enum.join()
  end

  defp compile_effect(_c, %Eff{op: :emit, args: %{"event" => ev}} = eff) do
    data = Map.get(eff.args, "data", "{}")
    payload = data |> to_string() |> Logos.Data.decode()
    json = if is_map(payload), do: Logos.Data.encode(payload), else: to_string(payload)
    ["    emit Emitted(", lit(ev), ", ", lit(json), ");\n"] |> IO.iodata_to_binary()
  end

  defp compile_effect(_c, %Eff{op: :set, args: %{"field" => f, "value" => v}}) do
    ["    ", f, " = ", lit(v), ";\n"] |> IO.iodata_to_binary()
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

  # For Solidity backend, we don't actually call tokens—just record intent.
  defp compile_effect(_c, %Eff{op: :transfer, args: args}) when is_map(args) do
    asset = Map.get(args, "asset", "0x0000000000000000000000000000000000000000")
    from_ = Map.get(args, "from")
    to_ = Map.get(args, "to", "0x0000000000000000000000000000000000000000")
    amount = Map.get(args, "amount", 0)
    from_expr = if from_ in [nil, "CALLER"], do: "msg.sender", else: solidity_addr_for_event(from_)

    [
      "    (bool ok, ) = ",
      solidity_addr_for_event(asset),
      ".call(abi.encodeWithSignature(\"transferFrom(address,address,uint256)\", ",
      from_expr,
      ", ",
      solidity_addr_for_event(to_),
      ", ",
      lit(amount),
      "));\n",
      "    require(ok, \"transferFrom\");\n",
      "    emit TransferNote(",
      solidity_addr_for_event(asset),
      ", ",
      from_expr,
      ", ",
      solidity_addr_for_event(to_),
      ", ",
      lit(amount),
      ");\n"
    ]
    |> IO.iodata_to_binary()
  end

  # ------------------------------- helpers -----------------------------------

  defp indent(text, spaces) when is_binary(text) do
    pad = String.duplicate(" ", spaces)

    text
    |> String.split("\n", trim: false)
    |> Enum.map(fn
      "" -> ""
      line -> pad <> line
    end)
    |> Enum.join("\n")
  end

  # Literal rendering for Solidity source
  defp lit(true), do: "true"
  defp lit(false), do: "false"
  defp lit(i) when is_integer(i), do: Integer.to_string(i)
  defp lit(<<"0x", _::binary>> = a), do: a
  defp lit({:raw, s}) when is_binary(s), do: s
  defp lit({:some, inner}), do: lit(inner)
  defp lit(:none), do: "0"
  defp lit({:decimal, coef, _scale}), do: Integer.to_string(coef)
  defp lit(s) when is_binary(s), do: "\"" <> String.replace(s, "\"", "\\\"") <> "\""
  defp lit(other), do: "\"" <> String.replace(inspect(other), "\"", "\\\"") <> "\""

  # For events, we need valid Solidity expressions for addresses
  defp solidity_addr_for_event(<<"0x", _::binary>> = a), do: a
  defp solidity_addr_for_event({:raw, s}) when is_binary(s), do: s
  defp solidity_addr_for_event(other) when is_binary(other), do: solidity_addr(other)
  defp solidity_addr_for_event(_), do: "address(0)"
end
