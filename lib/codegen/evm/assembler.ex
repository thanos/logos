defmodule Logos.Codegen.EVM.Assembler do
  @moduledoc false
  # Tiny two-pass assembler with labels and PUSH1..PUSH32 selection.
  # Instruction forms:
  #   {:label, :L0}
  #   {:op, :SSTORE}
  #   {:push, <<raw_bytes>>}   # exact bytes to push (we pick PUSHn)
  #   {:push_u, non_neg_integer()} # integer -> big-endian bytes
  #   {:jump_label, :L0}       # will encode as PUSH2 <addr> ; JUMP
  #   {:jumpi_label, :L0}      # will encode as PUSH2 <addr> ; JUMPI
  #
  # Returns binary bytecode.

  @op %{
    STOP: 0x00,
    ADD: 0x01,
    MUL: 0x02,
    SUB: 0x03,
    DIV: 0x04,
    SDIV: 0x05,
    MOD: 0x06,
    SMOD: 0x07,
    ADDMOD: 0x08,
    MULMOD: 0x09,
    EXP: 0x0A,
    SIGNEXTEND: 0x0B,
    LT: 0x10,
    GT: 0x11,
    SLT: 0x12,
    SGT: 0x13,
    EQ: 0x14,
    ISZERO: 0x15,
    AND: 0x16,
    OR: 0x17,
    XOR: 0x18,
    NOT: 0x19,
    BYTE: 0x1A,
    SHL: 0x1B,
    SHR: 0x1C,
    SAR: 0x1D,
    SHA3: 0x20,
    ADDRESS: 0x30,
    BALANCE: 0x31,
    ORIGIN: 0x32,
    CALLER: 0x33,
    CALLVALUE: 0x34,
    CALLDATALOAD: 0x35,
    CALLDATASIZE: 0x36,
    CALLDATACOPY: 0x37,
    CODESIZE: 0x38,
    CODECOPY: 0x39,
    GASPRICE: 0x3A,
    EXTCODESIZE: 0x3B,
    EXTCODECOPY: 0x3C,
    RETURNDATASIZE: 0x3D,
    RETURNDATACOPY: 0x3E,
    BLOCKHASH: 0x40,
    COINBASE: 0x41,
    TIMESTAMP: 0x42,
    NUMBER: 0x43,
    DIFFICULTY: 0x44,
    GASLIMIT: 0x45,
    CHAINID: 0x46,
    SELFBALANCE: 0x47,
    BASEFEE: 0x48,
    POP: 0x50,
    MLOAD: 0x51,
    MSTORE: 0x52,
    MSTORE8: 0x53,
    SLOAD: 0x54,
    SSTORE: 0x55,
    JUMP: 0x56,
    JUMPI: 0x57,
    PC: 0x58,
    MSIZE: 0x59,
    GAS: 0x5A,
    JUMPDEST: 0x5B,
    PUSH0: 0x5F,
    DUP1: 0x80,
    DUP2: 0x81,
    DUP3: 0x82,
    DUP4: 0x83,
    DUP5: 0x84,
    DUP6: 0x85,
    DUP7: 0x86,
    DUP8: 0x87,
    DUP9: 0x88,
    DUP10: 0x89,
    DUP11: 0x8A,
    DUP12: 0x8B,
    DUP13: 0x8C,
    DUP14: 0x8D,
    DUP15: 0x8E,
    DUP16: 0x8F,
    SWAP1: 0x90,
    SWAP2: 0x91,
    SWAP3: 0x92,
    SWAP4: 0x93,
    SWAP5: 0x94,
    SWAP6: 0x95,
    SWAP7: 0x96,
    SWAP8: 0x97,
    SWAP9: 0x98,
    SWAP10: 0x99,
    SWAP11: 0x9A,
    SWAP12: 0x9B,
    SWAP13: 0x9C,
    SWAP14: 0x9D,
    SWAP15: 0x9E,
    SWAP16: 0x9F,
    LOG0: 0xA0,
    LOG1: 0xA1,
    LOG2: 0xA2,
    LOG3: 0xA3,
    LOG4: 0xA4,
    CREATE: 0xF0,
    CALL: 0xF1,
    CALLCODE: 0xF2,
    RETURN: 0xF3,
    DELEGATECALL: 0xF4,
    CREATE2: 0xF5,
    STATICCALL: 0xFA,
    REVERT: 0xFD,
    INVALID: 0xFE,
    SELFDESTRUCT: 0xFF
  }

  def assemble(instrs) do
    {with_addrs, labels} = resolve_labels(instrs)
    encode(with_addrs, labels)
  end

  defp resolve_labels(instrs) do
    # First pass: compute offsets (assuming PUSH sizes are not yet known -> pessimistically assume PUSH2 for label placeholders)
    {_, labels, sized} =
      Enum.reduce(instrs, {0, %{}, []}, fn i, {pc, lab, acc} ->
        case i do
          {:label, name} ->
            # keep a JUMPDEST to anchor
            {pc, Map.put(lab, name, pc), [{:op, :JUMPDEST} | acc]}

          {:jump_label, l} ->
            {pc + 1 + 2 + 1, lab, [{:op, :JUMP}, {:push_u, {:label, l, 2}} | acc]}

          {:jumpi_label, l} ->
            {pc + 1 + 2 + 1, lab, [{:op, :JUMPI}, {:push_u, {:label, l, 2}} | acc]}

          {:push_u, {:label, _l, n}} ->
            {pc + 1 + n, lab, [i | acc]}

          {:push, bin} ->
            {pc + 1 + byte_size(bin), lab, [i | acc]}

          {:push_u, int} when is_integer(int) and int >= 0 ->
            n = int_size(int)
            {pc + 1 + n, lab, [i | acc]}

          {:op, op} ->
            {pc + 1, lab, [i | acc]}

          other when is_list(other) ->
            # allow nested sequences
            {delta, lab2, enc} = resolve_labels(other)
            {pc + delta, Map.merge(lab, lab2), enc ++ acc}
        end
      end)

    {Enum.reverse(sized), labels}
  end

  defp int_size(0), do: 1

  defp int_size(i) do
    :math.ceil(:math.log2(i + 1)) |> trunc() |> then(fn b -> div(max(b, 1) + 7, 8) end)
  end

  defp encode(list, labels) do
    list
    |> Enum.map(fn
      {:op, :JUMPDEST} ->
        <<@op[:JUMPDEST]>>

      {:op, k} ->
        <<Map.fetch!(@op, k)>>

      {:push, bin} ->
        n = byte_size(bin)
        if n < 1 or n > 32, do: raise("PUSH size #{n} invalid")
        <<0x5F + n, bin::binary>>

      {:push_u, {:label, l, n}} ->
        addr = Map.fetch!(labels, l)
        bin = :binary.encode_unsigned(addr) |> left_pad(n)
        <<0x5F + n, bin::binary>>

      {:push_u, int} when is_integer(int) and int >= 0 ->
        n = int_size(int)
        bin = :binary.encode_unsigned(int)
        bin = left_pad(bin, n)
        <<0x5F + n, bin::binary>>

      other when is_list(other) ->
        encode(other, labels)
    end)
    |> IO.iodata_to_binary()
  end

  defp left_pad(bin, n) do
    need = max(n - byte_size(bin), 0)
    :binary.copy(<<0>>, need) <> bin
  end
end
