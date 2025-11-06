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

  # ---- Runtime: dispatcher + clause bodies ----------------------------------
  defp build_runtime(%Contract{} = c, layout) do
    selector_map =
      for %Clause{name: n} <- c.clauses, into: %{} do
        # 0-arg external signature
        sel = keccak4("#{n}()")
        {n, sel}
      end

    clause_blocks =
      Enum.flat_map(c.clauses, fn cl ->
        body = compile_clause(c, cl, layout)
        [{:label, String.to_atom("clause_" <> cl.name)}, body]
      end)

    # Dispatcher:
    # s = (calldataload(0) >> 224)
    # if s == sel_i jump to clause_i
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
      # s := shr(224, calldataload(0))
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
        # build combined predicate; if false -> remedies
        pred = compile_pred_conj(prov, layout)

        [
          # stack: cond
          pred,
          # if cond -> OK
          {:jumpi_label, ok_lbl},
          compile_effect(c, rem, layout),
          {:jump_label, end_lbl},
          {:label, ok_lbl}
        ]
      else
        # require ...; if false -> revert
        case prov do
          [] ->
            []

          _ ->
            pred = compile_pred_conj(prov, layout)

            [
              # cond
              pred,
              # if cond -> OK
              {:jumpi_label, ok_lbl},
              {:op, :PUSH0},
              {:op, :PUSH0},
              {:op, :REVERT},
              {:label, ok_lbl}
            ]
        end
      end

    [
      # harmless double label; ensures a jumpdest before body
      {:label, ok_lbl},
      cond_block,
      compile_effect(c, shall, layout),
      {:label, end_lbl},
      {:op, :STOP}
    ]
  end

  # ---- Predicates (subset) ---------------------------------------------------
  # Accept lines like:
  #   submitted == true
  #   accepted != false
  # Combine with 'and'/'or'/'not' translated by the DSL -> pass raw text here.

  defp compile_pred_conj([], _layout), do: [{:push_u, 1}]

  defp compile_pred_conj(lines, layout) do
    # split on 'and'/'or' (left-to-right, no parens)
    tokens =
      lines
      |> Enum.map(&String.trim/1)
      |> Enum.join(" and ")
      |> String.replace(~r/\s+/, " ")
      |> String.split(" ")

    # Very small LL(1): handle 'not', 'and', 'or' with comparisons as atoms
    {stack, op} = parse_bool_expr(tokens, layout)
    stack ++ op
  end

  defp parse_bool_expr(tokens, layout) do
    # returns {code, trailer_code}
    parse_or(tokens, layout)
  end

  defp parse_or(tokens, layout) do
    {code1, rest} = parse_and(tokens, layout)
    do_parse_or(code1, rest, layout)
  end

  defp do_parse_or(code_acc, ["or" | t], layout) do
    {code2, rest} = parse_and(t, layout)
    # or: (a || b) -> (a + b) > 0 (but easier: a OR b with bitwise OR)
    {code_acc ++ code2 ++ [{:op, :OR}], rest}
    |> then(fn {c, r} -> do_parse_or(c, r, layout) end)
  end

  defp do_parse_or(code_acc, rest, _layout), do: {code_acc, rest}

  defp parse_and(tokens, layout) do
    {code1, rest} = parse_atom(tokens, layout)
    do_parse_and(code1, rest, layout)
  end

  defp do_parse_and(code_acc, ["and" | t], layout) do
    {code2, rest} = parse_atom(t, layout)

    {code_acc ++ code2 ++ [{:op, :AND}], rest}
    |> then(fn {c, r} -> do_parse_and(c, r, layout) end)
  end

  defp do_parse_and(code_acc, rest, _layout), do: {code_acc, rest}

  defp parse_atom(["not" | t], layout) do
    {code, rest} = parse_atom(t, layout)
    {code ++ [{:op, :ISZERO}], rest}
  end

  defp parse_atom(tokens, layout) do
    # expect: <field> (==|!=) <literal>
    [field, cmp, literal | rest] = tokens
    slot = Map.fetch!(layout, field)
    val = parse_lit(literal)

    code =
      [
        # SLOAD(slot)
        {:push_u, slot},
        {:op, :SLOAD},
        # const
        {:push_u, val},
        # equals?
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

  # unsupported effects (emit/transfer) -> no-ops in this backend
  defp compile_effect(_c, %Eff{op: other}, _layout) when other in [:emit, :transfer],
    do: []

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

    # Copy runtime code from "code" area into memory and RETURN it.
    # Standard pattern:
    #   PUSH <size>
    #   PUSH <offset>
    #   PUSH 0
    #   CODECOPY
    #   PUSH <size>
    #   PUSH 0
    #   RETURN
    runtime_size = byte_size(runtime)

    # We embed the runtime right after constructor and compute offsets by iteration until stable.
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

    # Iteratively fix the offset since PUSH sizes depend on the value itself.
    # First assemble a temporary constructor with offset 0 to get length, then recompute with correct offset.
    tmp = A.assemble(constructor_base.(0))
    offset1 = byte_size(tmp)
    cons1 = A.assemble(constructor_base.(offset1))
    # Verify if size changed; recompute once more if needed (usually settles)
    offset2 = byte_size(cons1)

    cons =
      if offset2 == offset1 do
        cons1
      else
        A.assemble(constructor_base.(offset2))
      end

    cons <> runtime
  end

  # ---- helpers ---------------------------------------------------------------
  defp keccak4(sig) do
    <<a, b, c, d, _rest::binary>> = :keccakf1600.sha3_256(sig)
    :binary.decode_unsigned(<<a, b, c, d>>)
  end

  defp uniq(prefix), do: String.to_atom("#{prefix}_#{System.unique_integer([:monotonic])}")
end
