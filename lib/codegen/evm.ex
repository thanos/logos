defmodule Logos.Codegen.EVM do
  @moduledoc false
  alias Logos.AST.{Contract, Clause, Eff}
  alias Logos.EVM.Assembler, as: A

  # Public API: returns %{runtime_hex: "0x...", creation_hex: "0x..."}.
  def emit(%Contract{} = c) do
    layout = storage_layout(c)
    runtime = build_runtime(c, layout)
    creation = build_creation(c, layout, runtime)

    %{
      runtime_hex: "0x" <> Base.encode16(runtime, case: :lower),
      creation_hex: "0x" <> Base.encode16(creation, case: :lower)
    }
  end

  # ---- Storage layout (simple, deterministic) -------------------------------
  defp storage_layout(%Contract{state: fields}) do
    fields
    |> Map.keys()
    |> Enum.sort()
    |> Enum.with_index()
    |> Map.new(fn {k, i} -> {k, i} end)
  end

  # ---- Runtime: selector dispatcher & clause bodies -------------------------
  defp build_runtime(%Contract{} = c, layout) do
    selector_map =
      for %Clause{name: n} <- c.clauses, into: %{} do
        sel = keccak4("#{n}()")
        {n, sel}
      end

    clause_blocks =
      Enum.flat_map(c.clauses, fn cl ->
        body = compile_clause(c, cl, layout)
        [{:label, String.to_atom("clause_" <> cl.name)}, body]
      end)

    dispatch =
      Enum.flat_map(c.clauses, fn %Clause{name: n} ->
        [
          {:push_u, selector_map[n]},
          {:op, :EQ},
          {:jumpi_label, String.to_atom("clause_" <> n)}
        ]
      end) ++
        [
          # fallback: REVERT(0,0)
          {:op, :PUSH0},
          {:op, :PUSH0},
          {:op, :REVERT}
        ]

    header = [
      # selector := shr(224, calldataload(0))
      {:op, :PUSH1},
      <<0x00>>,
      {:op, :CALLDATALOAD},
      {:op, :PUSH1},
      <<0xE0>>,
      {:op, :SHR}
    ]

    body = header ++ dispatch ++ clause_blocks ++ [{:op, :STOP}]
    A.assemble(body)
  end

  defp compile_clause(
         %Contract{} = c,
         %Clause{name: _n, provided: prov, shall: shall, remedies: rem},
         layout
       ) do
    ok_lbl = uniq(:ok)
    end_lbl = uniq(:end)
    with_remedies? = not is_nil(rem)

    cond_block =
      if with_remedies? do
        pred = compile_pred_conj(prov, layout)

        [
          pred,
          {:jumpi_label, ok_lbl},
          compile_effect(c, rem, layout),
          {:jump_label, end_lbl},
          {:label, ok_lbl}
        ]
      else
        case prov do
          [] ->
            []

          _ ->
            pred = compile_pred_conj(prov, layout)

            [
              pred,
              {:jumpi_label, ok_lbl},
              {:op, :PUSH0},
              {:op, :PUSH0},
              {:op, :REVERT},
              {:label, ok_lbl}
            ]
        end
      end

    [
      {:label, ok_lbl},
      cond_block,
      compile_effect(c, shall, layout),
      {:label, end_lbl},
      {:op, :STOP}
    ]
  end

  # ---- Predicates (subset) ---------------------------------------------------
  defp compile_pred_conj([], _layout), do: [{:push_u, 1}]

  defp compile_pred_conj(lines, layout) do
    tokens =
      lines
      |> Enum.map(&String.trim/1)
      |> Enum.join(" and ")
      |> String.replace(~r/\s+/, " ")
      |> String.split(" ")

    {stack, op} = parse_bool_expr(tokens, layout)
    stack ++ op
  end

  defp parse_bool_expr(tokens, layout), do: parse_or(tokens, layout)

  defp parse_or(tokens, layout) do
    {code1, rest} = parse_and(tokens, layout)
    do_parse_or(code1, rest, layout)
  end

  defp do_parse_or(code_acc, ["or" | t], layout) do
    {code2, rest} = parse_and(t, layout)

    {code_acc ++ code2 ++ [{:op, :OR}], rest}
    |> then(fn {c, r} -> do_parse_or(c, r, layout) end)
  end

  defp do_parse_or(code_acc, rest, _), do: {code_acc, rest}

  defp parse_and(tokens, layout) do
    {code1, rest} = parse_atom(tokens, layout)
    do_parse_and(code1, rest, layout)
  end

  defp do_parse_and(code_acc, ["and" | t], layout) do
    {code2, rest} = parse_atom(t, layout)

    {code_acc ++ code2 ++ [{:op, :AND}], rest}
    |> then(fn {c, r} -> do_parse_and(c, r, layout) end)
  end

  defp do_parse_and(code_acc, rest, _), do: {code_acc, rest}

  defp parse_atom(["not" | t], layout) do
    {code, rest} = parse_atom(t, layout)
    {code ++ [{:op, :ISZERO}], rest}
  end

  defp parse_atom(tokens, layout) do
    [field, cmp, literal | rest] = tokens
    slot = Map.fetch!(layout, field)
    val = parse_lit(literal)

    code =
      [
        {:push_u, slot},
        {:op, :SLOAD},
        {:push_u, val},
        {:op, :EQ}
      ] ++
        case cmp do
          "==" -> []
          "!=" -> [{:op, :ISZERO}]
          other -> raise "Unsupported comparator #{other}"
        end

    {code, rest}
  rescue
    _ -> raise "Bad predicate atom near: #{Enum.join(tokens, " ")}"
  end

  defp parse_lit("true"), do: 1
  defp parse_lit("false"), do: 0

  defp parse_lit(int) do
    case Integer.parse(int) do
      {i, ""} -> i
      _ -> raise "Only bool/int literals supported in bytecode backend (got #{int})"
    end
  end

  # ---- Effects ---------------------------------------------------------------
  defp compile_effect(_c, %Eff{op: :noop}, _layout), do: []

  defp compile_effect(c, %Eff{op: :all, args: list}, layout),
    do: Enum.flat_map(list, &compile_effect(c, &1, layout))

  # NEW: Emit -> LOG2 with topics [keccak("Emitted(string)"), keccak(eventName)], empty data (0 bytes)
  defp compile_effect(_c, %Eff{op: :emit, args: %{"event" => ev_name}}, _layout) do
    topic_sig = keccak256_int("Emitted(string)")
    topic_name = keccak256_int(ev_name)

    [
      # memory region (offset=0, size=0) – no data payload for now
      {:op, :PUSH0},
      {:op, :PUSH0},
      # topics (top of stack is last pushed)
      {:push_u, topic_sig},
      {:push_u, topic_name},
      {:op, :LOG2}
    ]
  end

  defp compile_effect(_c, %Eff{op: :set, args: %{"field" => f, "value" => v}}, layout) do
    slot = Map.fetch!(layout, f)
    val = evm_encode(v)

    [
      {:push_u, val},
      {:push_u, slot},
      {:op, :SSTORE}
    ]
  end

  defp compile_effect(c, %Eff{op: :if, args: %{"cond" => cond, "then" => t, "else" => e}}, layout) do
    l_then = uniq(:then)
    l_end = uniq(:ifend)

    cond_code = compile_pred_conj([cond], layout)
    then_code = compile_effect(c, t, layout)
    else_code = compile_effect(c, e, layout)

    cond_code ++
      [
        {:jumpi_label, l_then}
      ] ++
      else_code ++
      [
        {:jump_label, l_end},
        {:label, l_then}
      ] ++
      then_code ++
      [
        {:label, l_end}
      ]
  end

  # (still a no-op here) – keep transfers in Solidity backend until you add CALL path
  defp compile_effect(_c, %Eff{op: :transfer}, _layout), do: []

  defp evm_encode(true), do: 1
  defp evm_encode(false), do: 0
  defp evm_encode(i) when is_integer(i), do: i
  defp evm_encode({:raw, _}), do: 0
  defp evm_encode(_), do: 0

  # ---- Creation (constructor) ------------------------------------------------
  defp build_creation(%Contract{state: st} = c, layout, runtime) do
    init_storage =
      st
      |> Enum.map(fn {k, v} ->
        slot = Map.fetch!(layout, k)
        val = evm_encode(v)
        [{:push_u, val}, {:push_u, slot}, {:op, :SSTORE}]
      end)
      |> List.flatten()

    runtime_size = byte_size(runtime)

    constructor_base = fn offset_guess ->
      [
        init_storage,
        {:push_u, runtime_size},
        {:push_u, offset_guess},
        {:op, :PUSH0},
        {:op, :CODECOPY},
        {:push_u, runtime_size},
        {:op, :PUSH0},
        {:op, :RETURN}
      ]
    end

    tmp = A.assemble(constructor_base.(0))
    offset1 = byte_size(tmp)
    cons1 = A.assemble(constructor_base.(offset1))
    offset2 = byte_size(cons1)
    cons = if offset2 == offset1, do: cons1, else: A.assemble(constructor_base.(offset2))
    cons <> runtime
  end

  # ---- helpers ---------------------------------------------------------------
  defp keccak4(sig) do
    <<a, b, c, d, _rest::binary>> = :keccakf1600.sha3_256(sig)
    :binary.decode_unsigned(<<a, b, c, d>>)
  end

  defp keccak256_int(s) when is_binary(s) do
    :keccakf1600.sha3_256(s) |> :binary.decode_unsigned()
  end

  defp uniq(prefix), do: String.to_atom("#{prefix}_#{System.unique_integer([:monotonic])}")
end
