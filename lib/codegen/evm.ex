defmodule Logos.Codegen.EVM do
  @moduledoc """
  Backend: emit **raw EVM bytecode** (both creation and runtime).

  Features:
    * Clause -> external function with ABI selector and static param decoding
    * `Provided That` with `== != < > <= >=`, **now/timestamp** via `TIMESTAMP`
    * Time arithmetic with `± seconds` and `Duration("7d")` or bare `7d/12h/…`
    * Effects: `Set`, `All`, `If`, `Emit` (LOG2 topics only), `Transfer` (ERC-20 `transferFrom`)
    * Atomicity: failures (predicate or call) revert the clause

  Returns `%{runtime_hex: \"0x…\", creation_hex: \"0x…\"}` suitable for deployment.
  """
  @moduledoc false
  alias Logos.AST.{Contract, Clause, Eff}
  alias Logos.EVM.Assembler, as: A

  # Public API
  def emit(%Contract{} = c) do
    layout = storage_layout(c)
    runtime = build_runtime(c, layout)
    creation = build_creation(c, layout, runtime)

    %{
      runtime_hex: "0x" <> Base.encode16(runtime, case: :lower),
      creation_hex: "0x" <> Base.encode16(creation, case: :lower)
    }
  end

  # ---- Storage layout -------------------------------------------------------
  defp storage_layout(%Contract{state: fields}) do
    fields
    |> Map.keys()
    |> Enum.sort()
    |> Enum.with_index()
    |> Map.new(fn {k, i} -> {k, i} end)
  end

  # ---- Runtime with ABI selectors ------------------------------------------
  defp build_runtime(%Contract{} = c, layout) do
    selector_map =
      for %Clause{name: n, params: ps} <- c.clauses, into: %{} do
        sig = "#{n}(#{Enum.map(ps, &abi_type/1) |> Enum.join(",")})"
        {n, keccak4(sig)}
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
          # fallback
          {:op, :PUSH0},
          {:op, :PUSH0},
          {:op, :REVERT}
        ]

    header = [{:push_u, 0}, {:op, :CALLDATALOAD}, {:push_u, 0xE0}, {:op, :SHR}]

    program = List.flatten(header ++ dispatch ++ clause_blocks ++ [{:op, :STOP}])
    {rewritten, pool} = lift_pool(program)
    code = assemble_with_pool(rewritten, pool)
    code
  end

  defp lift_pool(instrs) do
    {rev, pool} =
      Enum.reduce(instrs, {[], []}, fn
        {:data_ref, bin}, {acc, pool} ->
          {[{:pool_slot, length(pool)} | acc], pool ++ [bin]}

        other, {acc, pool} ->
          {[other | acc], pool}
      end)

    {Enum.reverse(rev), pool}
  end

  defp assemble_with_pool(instrs, []) do
    A.assemble(instrs)
  end

  defp assemble_with_pool(instrs, pool) do
    skeleton = A.assemble(fill_pool(instrs, pool, 0))
    code = A.assemble(fill_pool(instrs, pool, byte_size(skeleton)))
    code <> IO.iodata_to_binary(pool)
  end

  defp fill_pool(instrs, pool, base) do
    {offsets, _} =
      pool
      |> Enum.with_index()
      |> Enum.reduce({%{}, base}, fn {bin, i}, {map, at} ->
        {Map.put(map, i, at), at + byte_size(bin)}
      end)

    Enum.map(instrs, fn
      {:pool_slot, i} -> {:push, <<Map.fetch!(offsets, i)::16>>}
      other -> other
    end)
  end

  defp abi_type(%{type: :uint256}), do: "uint256"
  defp abi_type(%{type: :address}), do: "address"
  defp abi_type(%{type: :bool}), do: "bool"
  defp abi_type(%{type: :string}), do: "string"
  defp abi_type(%{type: :bytes}), do: "bytes"

  # ---- Clause compilation with param decoding -------------------------------
  defp compile_clause(
         %Contract{} = c,
         %Clause{name: _n, params: params, provided: prov, shall: shall, remedies: rem},
         layout
       ) do
    {decode_code, env} = build_param_decoder(params)

    ok_lbl = uniq(:ok)
    end_lbl = uniq(:end)

    cond_block =
      if rem do
        pred = compile_pred_conj(prov, layout, env)

        [
          pred,
          {:jumpi_label, ok_lbl},
          compile_effect(c, rem, layout, env),
          {:jump_label, end_lbl},
          {:label, ok_lbl}
        ]
      else
        case prov do
          [] ->
            []

          _ ->
            pred = compile_pred_conj(prov, layout, env)

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
      decode_code,
      {:label, ok_lbl},
      cond_block,
      compile_effect(c, shall, layout, env),
      {:label, end_lbl},
      {:op, :STOP}
    ]
  end

  defp build_param_decoder(params) do
    n = length(params)
    need = 4 + 32 * n
    ok_lbl = uniq(:abiok)

    short_lbl = uniq(:revert_short)

    size_check = [
      {:op, :CALLDATASIZE},
      {:push_u, need},
      # cds < need ?
      {:op, :LT},
      # if too small -> revert
      {:jumpi_label, short_lbl},
      {:label, ok_lbl}
    ]

    env =
      params
      |> Enum.with_index()
      |> Map.new(fn {%{name: nm, type: t}, i} ->
        {nm, {:cd, 4 + 32 * i, t}}
      end)

    decoder = [
      size_check,
      {:label, short_lbl},
      {:op, :PUSH0},
      {:op, :PUSH0},
      {:op, :REVERT},
      {:label, ok_lbl}
    ]

    {decoder, env}
  end

  # ---- Predicates with Time/TIMESTAMP + arithmetic on durations -------------
  defp compile_pred_conj([], _layout, _env), do: [{:push_u, 1}]

  defp compile_pred_conj(lines, layout, env) do
    # Normalize duration literals to seconds before tokenization (e.g., "7d" -> "604800")
    normalized =
      lines
      |> Enum.map(&normalize_durations/1)

    tokens =
      normalized
      |> Enum.map(&String.trim/1)
      |> Enum.join(" and ")
      |> String.replace("(", " ( ")
      |> String.replace(")", " ) ")
      |> String.replace(~r/\s+/, " ")
      |> String.split(" ")
      |> Enum.reject(&(&1 == ""))

    {stack, op} = parse_bool_expr(tokens, layout, env)
    stack ++ op
  end

  defp parse_bool_expr(tokens, layout, env), do: parse_or(tokens, layout, env)

  defp parse_or(tokens, layout, env) do
    {code1, rest} = parse_and(tokens, layout, env)
    do_parse_or(code1, rest, layout, env)
  end

  defp do_parse_or(code_acc, ["or" | t], layout, env) do
    {code2, rest} = parse_and(t, layout, env)

    {code_acc ++ code2 ++ [{:op, :OR}], rest}
    |> then(fn {c, r} -> do_parse_or(c, r, layout, env) end)
  end

  defp do_parse_or(code_acc, rest, _layout, _env), do: {code_acc, rest}

  defp parse_and(tokens, layout, env) do
    {code1, rest} = parse_atom(tokens, layout, env)
    do_parse_and(code1, rest, layout, env)
  end

  defp do_parse_and(code_acc, ["and" | t], layout, env) do
    {code2, rest} = parse_atom(t, layout, env)

    {code_acc ++ code2 ++ [{:op, :AND}], rest}
    |> then(fn {c, r} -> do_parse_and(c, r, layout, env) end)
  end

  defp do_parse_and(code_acc, rest, _layout, _env), do: {code_acc, rest}

  # Parse one comparison atom: <expr> <cmp> <expr>, where <expr> supports +/- int seconds
  defp parse_atom(["not" | t], layout, env) do
    {code, rest} = parse_atom(t, layout, env)
    {code ++ [{:op, :ISZERO}], rest}
  end

  defp parse_atom(tokens, layout, env) do
    # Find first comparator
    {lhs_toks, cmp, rhs_toks, rest} = split_on_comparator(tokens)

    lhs_code = compile_value_expr(lhs_toks, layout, env)
    rhs_code = compile_value_expr(rhs_toks, layout, env)

    comp =
      case cmp do
        "==" -> [{:op, :EQ}]
        "!=" -> [{:op, :EQ}, {:op, :ISZERO}]
        "<" -> [{:op, :LT}]
        ">" -> [{:op, :GT}]
        # !(a > b)
        "<=" -> [{:op, :GT}, {:op, :ISZERO}]
        # !(a < b)
        ">=" -> [{:op, :LT}, {:op, :ISZERO}]
        other -> raise "Unsupported comparator #{other}"
      end

    {lhs_code ++ rhs_code ++ comp, rest}
  rescue
    _ -> raise "Bad predicate atom near: #{Enum.join(tokens, " ")}"
  end

  # Split tokens into LHS, comparator, RHS, rest (rest begins at next 'and'/'or' or end)
  defp split_on_comparator(tokens) do
    cmp_idx =
      Enum.find_index(tokens, &(&1 in ["==", "!=", "<", ">", "<=", ">="])) ||
        raise "Comparator not found"

    cmp = Enum.at(tokens, cmp_idx)
    lhs = Enum.slice(tokens, 0, cmp_idx)

    # RHS extends until next 'and'/'or' or end
    rhs_tokens = Enum.drop(tokens, cmp_idx + 1)

    {rhs, rest} =
      case Enum.find_index(rhs_tokens, &(&1 in ["and", "or"])) do
        nil -> {rhs_tokens, []}
        j -> {Enum.slice(rhs_tokens, 0, j), Enum.drop(rhs_tokens, j)}
      end

    {lhs, cmp, rhs, rest}
  end

  # Compile <expr> where expr := term {(+|-) int}*
  defp compile_value_expr(tokens, layout, env) do
    tokens = Enum.reject(tokens, &(&1 == ""))

    # parse first term
    {code, rest} = parse_term(tokens, layout, env)
    do_fold_arith(code, rest, layout, env)
  end

  defp do_fold_arith(code_acc, [op | rest], layout, env) when op in ["+", "-"] do
    {rhs, rest2} = parse_term(rest, layout, env)
    arith = if op == "+", do: [{:op, :ADD}], else: [{:op, :SUB}]
    do_fold_arith(code_acc ++ rhs ++ arith, rest2, layout, env)
  end

  defp do_fold_arith(code_acc, rest, _layout, _env) do
    # stop if no "+/-" next or malformed
    case rest do
      [] -> code_acc
      [next | _] when next in ["and", "or"] -> code_acc
      # be permissive
      _ -> code_acc
    end
  end

  defp parse_term(["(" | rest], layout, env) do
    {inner, rest} = take_group(rest, 1, [])
    {compile_value_expr(inner, layout, env), rest}
  end

  # term := now | timestamp | block.timestamp | <state-field> | <param> | <int>
  defp parse_term([tok | rest], layout, env) do
    code =
      case String.downcase(tok) do
        "now" ->
          [{:op, :TIMESTAMP}]

        "timestamp" ->
          [{:op, :TIMESTAMP}]

        "block.timestamp" ->
          [{:op, :TIMESTAMP}]

        _ ->
          cond do
            Map.has_key?(layout, tok) ->
              slot = Map.fetch!(layout, tok)
              [{:push_u, slot}, {:op, :SLOAD}]

            Map.has_key?(env, tok) ->
              load_param(tok, env)

            match?({_, ""}, Integer.parse(tok)) ->
              {i, ""} = Integer.parse(tok)
              [{:push_u, i}]

            true ->
              # unknown symbol -> 0 (defensive)
              [{:push_u, 0}]
          end
      end

    {code, rest}
  end

  defp parse_term([], _layout, _env), do: {[{:push_u, 0}], []}

  defp take_group([")" | rest], 1, acc), do: {Enum.reverse(acc), rest}
  defp take_group(["(" | rest], depth, acc), do: take_group(rest, depth + 1, ["(" | acc])
  defp take_group([")" | rest], depth, acc), do: take_group(rest, depth - 1, [")" | acc])
  defp take_group([tok | rest], depth, acc), do: take_group(rest, depth, [tok | acc])

  # ---- Effects ---------------------------------------------------------------
  defp compile_effect(_c, %Eff{op: :noop}, _layout, _env), do: []

  defp compile_effect(c, %Eff{op: :all, args: list}, layout, env),
    do: Enum.flat_map(list, &compile_effect(c, &1, layout, env))

  # Emit: topics plus a CODECOPY of the JSON payload from the code-end string pool.
  defp compile_effect(_c, %Eff{op: :emit, args: args}, _layout, _env) do
    ev_name = to_string(args["event"])
    payload = emit_payload(args["data"])
    json = if is_map(payload), do: Logos.Data.encode(payload), else: to_string(payload)
    extra = payload |> address_topics() |> Enum.take(2)
    sig = keccak256_int("Emitted(string)")
    name = keccak256_int(ev_name)

    [
      {:push_u, byte_size(json)},
      {:data_ref, json},
      {:op, :PUSH0},
      {:op, :CODECOPY},
      {:op, :PUSH0},
      {:push_u, byte_size(json)},
      {:push_u, sig},
      {:push_u, name}
    ] ++ Enum.map(extra, &{:push_u, &1}) ++ [{:op, log_op(length(extra))}]
  end

  # ERC-20 transferFrom(from,to,amount) – args can be literals OR params
  defp compile_effect(_c, %Eff{op: :transfer, args: kv}, _layout, env) do
    sel_transferFrom = 0x23B872DD

    token_code = push_addr_or_param(Map.fetch!(kv, "asset"), env)
    from_code = push_from(Map.get(kv, "from"), env)
    to_code = push_addr_or_param(Map.fetch!(kv, "to"), env)
    amount_code = push_uint_or_param(Map.fetch!(kv, "amount"), env)

    fail_lbl = uniq(:xfer_fail)
    done_lbl = uniq(:xfer_done)

    [
      # selector @ [0..3]
      {:push_u, sel_transferFrom},
      {:push_u, 0xE0},
      {:op, :SHL},
      {:op, :PUSH0},
      {:op, :MSTORE},

      # arg1 from @ [32..63]
      from_code,
      {:push_u, 0x20},
      {:op, :MSTORE},
      # arg2 to   @ [64..95]
      to_code,
      {:push_u, 0x40},
      {:op, :MSTORE},
      # arg3 amt  @ [96..127]
      amount_code,
      {:push_u, 0x60},
      {:op, :MSTORE},

      # CALL(gas, token, 0, in=0, size=100, out=0, outsize=32)
      {:op, :GAS},
      token_code,
      {:op, :PUSH0},
      {:op, :PUSH0},
      {:push_u, 0x64},
      {:op, :PUSH0},
      {:push_u, 0x20},
      {:op, :CALL},
      {:op, :DUP1},
      {:op, :ISZERO},
      {:jumpi_label, fail_lbl},
      {:op, :POP},
      {:op, :RETURNDATASIZE},
      {:push_u, 0x20},
      {:op, :LT},
      {:jumpi_label, done_lbl},
      {:op, :PUSH0},
      {:op, :PUSH0},
      {:op, :RETURNDATACOPY},
      {:op, :PUSH0},
      {:op, :MLOAD},
      {:op, :ISZERO},
      {:jumpi_label, fail_lbl},
      {:label, done_lbl},
      transfer_log(kv),
      {:label, fail_lbl},
      {:op, :PUSH0},
      {:op, :PUSH0},
      {:op, :REVERT}
    ]
  end

  defp compile_effect(_c, %Eff{op: :set, args: %{"field" => f, "value" => v}}, layout, env) do
    slot = Map.fetch!(layout, f)
    value_code = push_uint_or_param(v, env)
    value_code ++ [{:push_u, slot}, {:op, :SSTORE}]
  end

  defp compile_effect(
         c,
         %Eff{op: :if, args: %{"cond" => cond, "then" => t, "else" => e}},
         layout,
         env
       ) do
    l_then = uniq(:then)
    l_end = uniq(:ifend)
    cond_code = compile_pred_conj([cond], layout, env)
    then_code = compile_effect(c, t, layout, env)
    else_code = compile_effect(c, e, layout, env)

    cond_code ++
      [{:jumpi_label, l_then}] ++
      else_code ++ [{:jump_label, l_end}, {:label, l_then}] ++ then_code ++ [{:label, l_end}]
  end

  # ---- Helpers: durations normalization -------------------------------------
  defp normalize_durations(line), do: Logos.Duration.rewrite(line)

  defp push_from(nil, _env), do: [{:op, :CALLER}]
  defp push_from("CALLER", _env), do: [{:op, :CALLER}]
  defp push_from({:raw, "CALLER"}, _env), do: [{:op, :CALLER}]
  defp push_from(other, env), do: push_addr_or_param(other, env)

  defp log_op(0), do: :LOG2
  defp log_op(1), do: :LOG3
  defp log_op(_), do: :LOG4

  defp emit_payload(nil), do: %{}
  defp emit_payload(data), do: data |> to_string() |> Logos.Data.decode()

  defp address_topics(map) when is_map(map) do
    map
    |> Enum.sort_by(fn {key, _} -> to_string(key) end)
    |> Enum.flat_map(fn {_key, value} ->
      case value do
        "0x" <> hex when byte_size(hex) == 40 ->
          case Base.decode16(hex, case: :mixed) do
            {:ok, bin} -> [:binary.decode_unsigned(bin)]
            _ -> []
          end

        _ ->
          []
      end
    end)
  end

  defp address_topics(_), do: []

  defp transfer_log(kv) do
    [
      {:op, :PUSH0},
      {:op, :PUSH0},
      {:push_u, keccak256_int("Transfer")},
      {:push_u, topic_word(kv["asset"])},
      {:push_u, topic_word(Map.get(kv, "from", "CALLER"))},
      {:op, :LOG3}
    ]
  end

  defp topic_word(<<"0x", hex::binary>> = value) do
    case Base.decode16(hex, case: :mixed) do
      {:ok, bin} -> :binary.decode_unsigned(bin)
      _ -> keccak256_int(value)
    end
  end

  defp topic_word(other), do: keccak256_int(to_string(other))

  # ---- Helpers: push immediates or load params ------------------------------
  defp push_uint_or_param(v, env) do
    case v do
      i when is_integer(i) ->
        [{:push_u, i}]

      {:decimal, coef, _scale} ->
        [{:push_u, coef}]

      {:some, inner} ->
        push_uint_or_param(inner, env)

      :none ->
        [{:push_u, 0}]

      true ->
        [{:push_u, 1}]

      false ->
        [{:push_u, 0}]

      {:raw, name} when is_binary(name) ->
        load_param(name, env)

      s when is_binary(s) ->
        case Integer.parse(s) do
          {i, ""} -> [{:push_u, i}]
          _ -> [{:push_u, 0}]
        end

      _ ->
        [{:push_u, 0}]
    end
  end

  defp push_addr_or_param(v, env) do
    case v do
      <<"0x", hex::binary>> ->
        case Base.decode16(String.upcase(hex), case: :mixed) do
          {:ok, bin} -> [{:push_u, :binary.decode_unsigned(bin)}]
          _ -> [{:push_u, 0}]
        end

      {:raw, name} when is_binary(name) ->
        load_param(name, env)

      _ ->
        [{:push_u, 0}]
    end
  end

  defp load_param(name, env) do
    case env[name] do
      {:cd, off, type} when type in [:string, :bytes] ->
        [
          {:push_u, off},
          {:op, :CALLDATALOAD},
          {:push_u, 4},
          {:op, :ADD},
          {:op, :CALLDATALOAD}
        ]

      {:cd, off, _type} ->
        [{:push_u, off}, {:op, :CALLDATALOAD}]

      nil ->
        [{:push_u, 0}]
    end
  end

  # ---- Creation (constructor) -----------------------------------------------
  defp build_creation(%Contract{state: st}, layout, runtime) do
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

  # ---- hashing / encoding helpers -------------------------------------------
  defp keccak4(sig) when is_binary(sig) do
    <<a, b, c, d, _::binary>> = :crypto.hash(:sha3_256, sig)
    :binary.decode_unsigned(<<a, b, c, d>>)
  end

  defp keccak256_int(s) when is_binary(s) do
    :crypto.hash(:sha3_256, s) |> :binary.decode_unsigned()
  end

  defp evm_encode(true), do: 1
  defp evm_encode(false), do: 0
  defp evm_encode(i) when is_integer(i), do: i
  defp evm_encode({:decimal, coef, _scale}), do: coef
  defp evm_encode({:some, inner}), do: evm_encode(inner)
  defp evm_encode(:none), do: 0
  defp evm_encode({:raw, _}), do: 0
  defp evm_encode(_), do: 0

  defp uniq(prefix), do: String.to_atom("#{prefix}_#{System.unique_integer([:monotonic])}")
end
