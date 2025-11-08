defmodule Logos.Codegen.EVM do
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

    header = [
      # selector := shr(224, calldataload(0))
      {:op, :PUSH1},
      <<0x00>>,
      {:op, :CALLDATALOAD},
      {:op, :PUSH1},
      <<0xE0>>,
      {:op, :SHR}
    ]

    A.assemble(header ++ dispatch ++ clause_blocks ++ [{:op, :STOP}])
  end

  defp abi_type(%{type: :uint256}), do: "uint256"
  defp abi_type(%{type: :address}), do: "address"
  defp abi_type(%{type: :bool}), do: "bool"

  # ---- Clause compilation with param decoding -------------------------------
  defp compile_clause(
         %Contract{} = c,
         %Clause{name: _n, params: params, provided: prov, shall: shall, remedies: rem},
         layout
       ) do
    # Build env = %{ "amount" => {:cd, offset, :uint256}, ... }
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
      # --- ABI param decoding first ---
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

    # calldata size >= need ?
    size_check = [
      {:op, :CALLDATASIZE},
      {:push_u, need},
      # cds < need ?
      {:op, :LT},
      # if too small -> revert
      {:jumpi_label, :_revert_short},
      {:label, ok_lbl}
    ]

    # Offsets
    env =
      params
      |> Enum.with_index()
      |> Map.new(fn {%{name: nm, type: t}, i} ->
        {nm, {:cd, 4 + 32 * i, t}}
      end)

    # We don't copy to memory; we load directly where needed.
    # Emit a revert label shared by all clauses (local in assembled block)
    decoder = [
      size_check,
      {:label, :_revert_short},
      {:op, :PUSH0},
      {:op, :PUSH0},
      {:op, :REVERT},
      {:label, ok_lbl}
    ]

    {decoder, env}
  end

  # ---- Predicates -----------------------------------------------------------
  defp compile_pred_conj([], _layout, _env), do: [{:push_u, 1}]

  defp compile_pred_conj(lines, layout, env) do
    tokens =
      lines
      |> Enum.map(&String.trim/1)
      |> Enum.join(" and ")
      |> String.replace(~r/\s+/, " ")
      |> String.split(" ")

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

  defp parse_atom(["not" | t], layout, env) do
    {code, rest} = parse_atom(t, layout, env)
    {code ++ [{:op, :ISZERO}], rest}
  end

  defp parse_atom(tokens, layout, env) do
    # allow either: <state_field> (==|!=) <literal|param>
    [lhs, cmp, rhs | rest] = tokens

    lhs_code =
      if Map.has_key?(layout, lhs) do
        slot = Map.fetch!(layout, lhs)
        [{:push_u, slot}, {:op, :SLOAD}]
      else
        # treat as param
        load_param(lhs, env)
      end

    rhs_code =
      case parse_rhs(rhs, env) do
        {:imm, i} -> [{:push_u, i}]
        {:code, code} -> code
      end

    code =
      lhs_code ++
        rhs_code ++
        [{:op, :EQ}] ++
        case cmp do
          "==" -> []
          "!=" -> [{:op, :ISZERO}]
          other -> raise "Unsupported comparator #{other}"
        end

    {code, rest}
  rescue
    _ -> raise "Bad predicate atom near: #{Enum.join(tokens, " ")}"
  end

  defp parse_rhs("true", _), do: {:imm, 1}
  defp parse_rhs("false", _), do: {:imm, 0}

  defp parse_rhs(int, _) do
    case Integer.parse(int) do
      {i, ""} -> {:imm, i}
      # fallback
      _ -> {:code, []}
    end
  end

  # ---- Effects ---------------------------------------------------------------
  defp compile_effect(_c, %Eff{op: :noop}, _layout, _env), do: []

  defp compile_effect(c, %Eff{op: :all, args: list}, layout, env),
    do: Enum.flat_map(list, &compile_effect(c, &1, layout, env))

  # Emit: LOG2 with [keccak("Emitted(string)"), keccak(eventName)], no data
  defp compile_effect(_c, %Eff{op: :emit, args: %{"event" => ev_name}}, _layout, _env) do
    sig = keccak256_int("Emitted(string)")
    name = keccak256_int(ev_name)
    [{:op, :PUSH0}, {:op, :PUSH0}, {:push_u, sig}, {:push_u, name}, {:op, :LOG2}]
  end

  # ERC-20 transferFrom(from,to,amount) – args can be literals OR params
  defp compile_effect(_c, %Eff{op: :transfer, args: kv}, _layout, env) do
    sel_transferFrom = 0x23B872DD

    token_code = push_addr_or_param(Map.fetch!(kv, "asset"), env)
    from_code = push_addr_or_param(Map.fetch!(kv, "from"), env)
    to_code = push_addr_or_param(Map.fetch!(kv, "to"), env)
    amount_code = push_uint_or_param(Map.fetch!(kv, "amount"), env)

    fail_lbl = uniq(:xfer_fail)
    done_lbl = uniq(:xfer_done)

    [
      # selector @ [0..3]
      {:push_u, sel_transferFrom},
      {:op, :PUSH1},
      <<0xE0>>,
      {:op, :SHL},
      {:op, :PUSH0},
      {:op, :MSTORE},

      # arg1 from @ [32..63]
      from_code,
      {:op, :PUSH1},
      <<0x20>>,
      {:op, :MSTORE},
      # arg2 to   @ [64..95]
      to_code,
      {:op, :PUSH1},
      <<0x40>>,
      {:op, :MSTORE},
      # arg3 amt  @ [96..127]
      amount_code,
      {:op, :PUSH1},
      <<0x60>>,
      {:op, :MSTORE},

      # CALL(gas, token, 0, in=0, size=100, out=0, outsize=32)
      {:op, :GAS},
      token_code,
      {:op, :PUSH0},
      {:op, :PUSH0},
      {:op, :PUSH1},
      <<0x64>>,
      {:op, :PUSH0},
      {:op, :PUSH1},
      <<0x20>>,
      {:op, :CALL},
      {:op, :DUP1},
      {:op, :ISZERO},
      {:jumpi_label, fail_lbl},
      {:op, :POP},
      {:op, :RETURNDATASIZE},
      {:op, :PUSH1},
      <<0x20>>,
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
      [],
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

  # ---- Helpers: push immediates or load params ------------------------------
  defp push_uint_or_param(v, env) do
    case v do
      i when is_integer(i) ->
        [{:push_u, i}]

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
      <<"0x", _::binary>> = addr ->
        case Base.decode16(String.upcase(String.slice(addr, 2..-1)), case: :mixed) do
          {:ok, bin} ->
            [{:push, bin |> :binary.decode_unsigned() |> :binary.encode_unsigned()}]
            |> List.first()
            |> case do
              # we want exact bytes left-padded automatically via PUSHn
              {:push, _} = p -> [p]
              _ -> [{:push_u, :binary.decode_unsigned(bin)}]
            end

          _ ->
            [{:push_u, 0}]
        end

      {:raw, name} when is_binary(name) ->
        # param (address or uint256) – just CALLDATALOAD slot
        load_param(name, env)

      _ ->
        [{:push_u, 0}]
    end
  end

  defp load_param(name, env) do
    case env[name] do
      {:cd, off, _type} -> [{:push_u, off}, {:op, :CALLDATALOAD}]
      nil -> [{:push_u, 0}]
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
  defp keccak4(sig) do
    <<a, b, c, d, _::binary>> = :keccakf1600.sha3_256(sig)
    :binary.decode_unsigned(<<a, b, c, d>>)
  end

  defp keccak256_int(s) when is_binary(s),
    do: :keccakf1600.sha3_256(s) |> :binary.decode_unsigned()

  defp evm_encode(true), do: 1
  defp evm_encode(false), do: 0
  defp evm_encode(i) when is_integer(i), do: i
  defp evm_encode({:raw, _}), do: 0
  defp evm_encode(_), do: 0

  defp uniq(prefix), do: String.to_atom("#{prefix}_#{System.unique_integer([:monotonic])}")
end
