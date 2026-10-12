# defmodule Logos.EVM.Assembler do
#   @moduledoc """
#   Minimal two-pass EVM assembler with labels and PUSHn selection.

#   Accepted instructions:
#     * `{:label, :L}`                  -> emits JUMPDEST
#     * `{:op, atom}`                   -> single opcode (must exist in table)
#     * `{:push, <<bytes::binary-size(1..32)>>}` -> raw PUSH
#     * `{:push_u, non_neg_integer()}`  -> unsigned integer (auto-chooses PUSHn, or PUSH0 for 0)
#     * `{:push_u, {:label, :L, n}}`    -> address of label L as `n`-byte big-endian
#     * `{:jump_label, :L}`             -> PUSH2 addr; JUMP
#     * `{:jumpi_label, :L}`            -> PUSH2 addr; JUMPI
#     * lists are flattened
#   """

#   @type instr ::
#           {:label, atom()}
#           | {:op, atom()}
#           | {:push, binary()}
#           | {:push_u, non_neg_integer() | {:label, atom(), pos_integer()}}
#           | {:jump_label, atom()}
#           | {:jumpi_label, atom()}
#           | [any()]

#   # --- opcode table (only what we need, add more as desired) -----------------
#   @op <<
#     # 0x00..
#     # STOP
#     0x00::8,
#     # ADD
#     0x01::8,
#     # MUL
#     0x02::8,
#     # SUB
#     0x03::8,
#     # DIV
#     0x04::8,
#     # LT
#     0x10::8,
#     # GT
#     0x11::8,
#     # EQ
#     0x14::8,
#     # ISZERO
#     0x15::8,
#     # AND
#     0x16::8,
#     # OR
#     0x17::8,
#     # NOT (rare)
#     0x19::8,
#     # CALLDATALOAD
#     0x35::8,
#     # CALLDATASIZE
#     0x36::8,
#     # CALLDATACOPY
#     0x37::8,
#     # CODECOPY
#     0x39::8,
#     # RETURNDATASIZE
#     0x3D::8,
#     # RETURNDATACOPY
#     0x3E::8,
#     # BLOCKHASH
#     0x40::8,
#     # TIMESTAMP   <-- IMPORTANT
#     0x42::8,
#     # NUMBER
#     0x43::8,
#     # POP
#     0x50::8,
#     # MLOAD
#     0x51::8,
#     # MSTORE
#     0x52::8,
#     # SLOAD
#     0x54::8,
#     # SSTORE
#     0x55::8,
#     # JUMP
#     0x56::8,
#     # JUMPI
#     0x57::8,
#     # JUMPDEST
#     0x5B::8,
#     # PUSH0       <-- IMPORTANT (Shanghai)
#     0x5F::8,
#     # PUSH1
#     0x60::8,
#     # PUSH2..PUSH32 are contiguous 0x61..0x7f
#     # DUP1
#     0x80::8,
#     # DUP2
#     0x81::8,
#     # SWAP1
#     0x90::8,
#     # LOG0
#     0xA0::8,
#     # LOG1
#     0xA1::8,
#     # LOG2
#     0xA2::8,
#     # CALL
#     0xF1::8,
#     # REVERT
#     0xFD::8,
#     # INVALID
#     0xFE::8
#   >>

#   @opcode_map %{
#     STOP: 0x00,
#     ADD: 0x01,
#     MUL: 0x02,
#     SUB: 0x03,
#     DIV: 0x04,
#     LT: 0x10,
#     GT: 0x11,
#     EQ: 0x14,
#     ISZERO: 0x15,
#     AND: 0x16,
#     OR: 0x17,
#     NOT: 0x19,
#     CALLDATALOAD: 0x35,
#     CALLDATASIZE: 0x36,
#     CALLDATACOPY: 0x37,
#     CODECOPY: 0x39,
#     RETURNDATASIZE: 0x3D,
#     RETURNDATACOPY: 0x3E,
#     BLOCKHASH: 0x40,
#     TIMESTAMP: 0x42,
#     NUMBER: 0x43,
#     POP: 0x50,
#     MLOAD: 0x51,
#     MSTORE: 0x52,
#     SLOAD: 0x54,
#     SSTORE: 0x55,
#     JUMP: 0x56,
#     JUMPI: 0x57,
#     JUMPDEST: 0x5B,
#     PUSH0: 0x5F,
#     PUSH1: 0x60,
#     DUP1: 0x80,
#     DUP2: 0x81,
#     SWAP1: 0x90,
#     LOG0: 0xA0,
#     LOG1: 0xA1,
#     LOG2: 0xA2,
#     CALL: 0xF1,
#     REVERT: 0xFD,
#     INVALID: 0xFE
#   }

#   @doc "Assemble a list of instructions into a binary of EVM bytecode."
#   @spec assemble([instr]) :: binary()
#   def assemble(instrs) do
#     flat = List.flatten(instrs)
#     {with_placeholders, labels} = resolve_labels(flat)
#     encode(with_placeholders, labels)
#   end

#   # --------------------------- pass 1: resolve labels ------------------------

#   defp resolve_labels(instrs) do
#     {_, pos, labels, out} =
#       Enum.reduce(instrs, {0, 0, %{}, []}, fn ins, {i, pc, map, acc} ->
#         case ins do
#           {:label, name} ->
#             # JUMPDEST consumes 1 byte
#             {i + 1, pc + 1, Map.put(map, name, pc), [{:op, :JUMPDEST} | acc]}

#           {:jump_label, name} ->
#             # placeholder: PUSH2 addr; JUMP  -> 1 + 2 + 1 bytes
#             {i + 1, pc + 3, map, [{:jump_label, name} | acc]}

#           {:jumpi_label, name} ->
#             {i + 1, pc + 3, map, [{:jumpi_label, name} | acc]}

#           {:push_u, {:label, name, n}} when n in 1..32 ->
#             # placeholder PUSHn addr
#             {i + 1, pc + 1 + n, map, [{:push_u, {:label, name, n}} | acc]}

#           {:op, :PUSH0} ->
#             {i + 1, pc + 1, map, [ins | acc]}

#           {:op, op} ->
#             code = opcode!(op)
#             {i + 1, pc + 1, map, [ins | acc]}

#           {:push, bin} when is_binary(bin) and byte_size(bin) in 1..32 ->
#             {i + 1, pc + 1 + byte_size(bin), map, [ins | acc]}

#           {:push_u, int} when is_integer(int) and int >= 0 ->
#             size = int_size(int)
#             size = if size == 0, do: 0, else: size
#             pc_inc = if size == 0, do: 1, else: 1 + size
#             {i + 1, pc + pc_inc, map, [ins | acc]}

#           other ->
#             raise "Unsupported instruction in pass1: #{inspect(other)}"
#         end
#       end)

#     {Enum.reverse(out), labels}
#   end

#   # ---------------------------- pass 2: encode --------------------------------

#   defp encode(instrs, labels) do
#     IO.iodata_to_binary(Enum.map(instrs, &encode1(&1, labels)))
#   end

#   defp encode1({:op, op}, _labels) do
#     <<opcode!(op)>>
#   end

#   defp encode1({:push, bin}, _labels) when byte_size(bin) in 1..32 do
#     n = byte_size(bin)
#     <<0x60 + (n - 1), bin::binary>>
#   end

#   defp encode1({:push_u, 0}, _labels) do
#     <<opcode!(:PUSH0)>>
#   end

#   defp encode1({:push_u, int}, _labels) when is_integer(int) and int > 0 do
#     bin = :binary.encode_unsigned(int)
#     n = byte_size(bin)
#     <<0x60 + (n - 1), bin::binary>>
#   end

#   defp encode1({:push_u, {:label, name, n}}, labels) when n in 1..32 do
#     addr = Map.fetch!(labels, name)
#     bin = :binary.encode_unsigned(addr) |> left_pad(n)
#     <<0x60 + (n - 1), bin::binary>>
#   end

#   defp encode1({:jump_label, name}, labels) do
#     addr = Map.fetch!(labels, name)
#     <<0x61, left_pad(:binary.encode_unsigned(addr), 2)::binary, opcode!(:JUMP)>>
#   end

#   defp encode1({:jumpi_label, name}, labels) do
#     addr = Map.fetch!(labels, name)
#     <<0x61, left_pad(:binary.encode_unsigned(addr), 2)::binary, opcode!(:JUMPI)>>
#   end

#   defp encode1(list, labels) when is_list(list) do
#     IO.iodata_to_binary(Enum.map(list, &encode1(&1, labels)))
#   end

#   defp encode1(other, _labels), do: raise("Unsupported instruction in pass2: #{inspect(other)}")

#   # -------------------------------- helpers ----------------------------------

#   defp opcode!(op_atom) do
#     case @opcode_map[op_atom] do
#       nil -> raise "Unknown opcode: #{inspect(op_atom)}"
#       b -> b
#     end
#   end

#   defp int_size(0), do: 0

#   defp int_size(int) do
#     byte_size(:binary.encode_unsigned(int))
#   end

#   defp left_pad(bin, n) when is_integer(n), do: left_pad(bin, n - byte_size(bin))
#   defp left_pad(bin, k) when k <= 0, do: bin
#   defp left_pad(bin, k), do: :binary.copy(<<0>>, k) <> bin
# end

defmodule Logos.EVM.Assembler do
  @moduledoc """
  Minimal two-pass EVM assembler with labels and PUSHn selection.

  Accepted instructions:
    * `{:label, :L}`                  -> emits JUMPDEST
    * `{:op, atom}`                   -> single opcode (must exist in table)
    * `{:push, <<bytes::binary-size(1..32)>>}` -> raw PUSH
    * `{:push_u, non_neg_integer()}`  -> unsigned integer (auto-chooses PUSHn, or PUSH0 for 0)
    * `{:push_u, {:label, :L, n}}`    -> address of label L as `n`-byte big-endian
    * `{:jump_label, :L}`             -> PUSH2 addr; JUMP
    * `{:jumpi_label, :L}`            -> PUSH2 addr; JUMPI
    * lists are flattened
  """

  @type instr ::
          {:label, atom()}
          | {:op, atom()}
          | {:push, binary()}
          | {:push_u, non_neg_integer() | {:label, atom(), pos_integer()}}
          | {:jump_label, atom()}
          | {:jumpi_label, atom()}
          | [any()]

  # --- opcode table (only what we need, add more as desired) -----------------
  @op <<
    # 0x00..
    # STOP
    0x00::8,
    # ADD
    0x01::8,
    # MUL
    0x02::8,
    # SUB
    0x03::8,
    # DIV
    0x04::8,
    # LT
    0x10::8,
    # GT
    0x11::8,
    # EQ
    0x14::8,
    # ISZERO
    0x15::8,
    # AND
    0x16::8,
    # OR
    0x17::8,
    # NOT (rare)
    0x19::8,
    # CALLDATALOAD
    0x35::8,
    # CALLDATASIZE
    0x36::8,
    # CALLDATACOPY
    0x37::8,
    # CODECOPY
    0x39::8,
    # RETURNDATASIZE
    0x3D::8,
    # RETURNDATACOPY
    0x3E::8,
    # BLOCKHASH
    0x40::8,
    # TIMESTAMP   <-- IMPORTANT
    0x42::8,
    # NUMBER
    0x43::8,
    # POP
    0x50::8,
    # MLOAD
    0x51::8,
    # MSTORE
    0x52::8,
    # SLOAD
    0x54::8,
    # SSTORE
    0x55::8,
    # JUMP
    0x56::8,
    # JUMPI
    0x57::8,
    # JUMPDEST
    0x5B::8,
    # PUSH0       <-- IMPORTANT (Shanghai)
    0x5F::8,
    # PUSH1
    0x60::8,
    # PUSH2..PUSH32 are contiguous 0x61..0x7f
    # DUP1
    0x80::8,
    # DUP2
    0x81::8,
    # SWAP1
    0x90::8,
    # LOG0
    0xA0::8,
    # LOG1
    0xA1::8,
    # LOG2
    0xA2::8,
    # CALL
    0xF1::8,
    # REVERT
    0xFD::8,
    # INVALID
    0xFE::8
  >>

  @opcode_map %{
    STOP: 0x00,
    ADD: 0x01,
    MUL: 0x02,
    SUB: 0x03,
    DIV: 0x04,
    LT: 0x10,
    GT: 0x11,
    SHL: 0x1B,
    SHR: 0x1C,
    EQ: 0x14,
    ISZERO: 0x15,
    AND: 0x16,
    OR: 0x17,
    NOT: 0x19,
    CALLER: 0x33,
    CALLDATALOAD: 0x35,
    CALLDATASIZE: 0x36,
    CALLDATACOPY: 0x37,
    CODECOPY: 0x39,
    RETURNDATASIZE: 0x3D,
    RETURNDATACOPY: 0x3E,
    BLOCKHASH: 0x40,
    TIMESTAMP: 0x42,
    NUMBER: 0x43,
    POP: 0x50,
    MLOAD: 0x51,
    MSTORE: 0x52,
    SLOAD: 0x54,
    SSTORE: 0x55,
    GAS: 0x5A,
    JUMP: 0x56,
    JUMPI: 0x57,
    JUMPDEST: 0x5B,
    PUSH0: 0x5F,
    PUSH1: 0x60,
    DUP1: 0x80,
    DUP2: 0x81,
    SWAP1: 0x90,
    LOG0: 0xA0,
    LOG1: 0xA1,
    LOG2: 0xA2,
    LOG3: 0xA3,
    LOG4: 0xA4,
    CALL: 0xF1,
    RETURN: 0xF3,
    REVERT: 0xFD,
    INVALID: 0xFE
  }

  @doc "Assemble a list of instructions into a binary of EVM bytecode."
  @spec assemble([instr]) :: binary()
  def assemble(instrs) do
    flat = List.flatten(instrs)
    {with_placeholders, labels} = resolve_labels(flat)
    encode(with_placeholders, labels)
  end

  # --------------------------- pass 1: resolve labels ------------------------

  defp resolve_labels(instrs) do
    widths = %{}
    {out, labels} = layout(instrs, widths)
    converge(instrs, widths, out, labels, 0)
  end

  defp converge(_instrs, _widths, out, labels, 32), do: {out, labels}

  defp converge(instrs, widths, out, labels, n) do
    next =
      instrs
      |> Enum.with_index()
      |> Enum.reduce(widths, fn
        {{kind, name}, i}, acc when kind in [:jump_label, :jumpi_label] ->
          Map.put(acc, i, jump_width(Map.fetch!(labels, name)))

        _, acc ->
          acc
      end)

    if next == widths do
      {out, labels}
    else
      {out2, labels2} = layout(instrs, next)
      converge(instrs, next, out2, labels2, n + 1)
    end
  end

  defp layout(instrs, widths) do
    {_, _pos, labels, out} =
      Enum.reduce(Enum.with_index(instrs), {0, 0, %{}, []}, fn {ins, i}, {_, pc, map, acc} ->
        case ins do
          {:label, name} ->
            {i, pc + 1, Map.put(map, name, pc), [{:op, :JUMPDEST} | acc]}

          {:jump_label, name} ->
            {i, pc + Map.get(widths, i, 3), map, [{:jump_label, name} | acc]}

          {:jumpi_label, name} ->
            {i, pc + Map.get(widths, i, 3), map, [{:jumpi_label, name} | acc]}

          {:push_u, {:label, _name, n}} when n in 1..32 ->
            {i, pc + 1 + n, map, [ins | acc]}

          {:op, :PUSH0} ->
            {i, pc + 1, map, [ins | acc]}

          {:op, _op} ->
            {i, pc + 1, map, [ins | acc]}

          {:push, bin} when is_binary(bin) and byte_size(bin) in 1..32 ->
            {i, pc + 1 + byte_size(bin), map, [ins | acc]}

          {:push_u, int} when is_integer(int) and int >= 0 ->
            size = int_size(int)
            pc_inc = if size == 0, do: 1, else: 1 + size
            {i, pc + pc_inc, map, [ins | acc]}

          other ->
            raise "Unsupported instruction in pass1: #{inspect(other)}"
        end
      end)

    {Enum.reverse(out), labels}
  end

  defp jump_width(addr) when addr <= 255, do: 3
  defp jump_width(addr) when addr <= 65_535, do: 4
  defp jump_width(_addr), do: 6

  # ---------------------------- pass 2: encode --------------------------------

  defp encode(instrs, labels) do
    IO.iodata_to_binary(Enum.map(instrs, &encode1(&1, labels)))
  end

  defp encode1({:op, op}, _labels) do
    <<opcode!(op)>>
  end

  defp encode1({:push, bin}, _labels) when byte_size(bin) in 1..32 do
    n = byte_size(bin)
    <<0x60 + (n - 1), bin::binary>>
  end

  defp encode1({:push_u, 0}, _labels) do
    <<opcode!(:PUSH0)>>
  end

  defp encode1({:push_u, int}, _labels) when is_integer(int) and int > 0 do
    bin = :binary.encode_unsigned(int)
    n = byte_size(bin)
    <<0x60 + (n - 1), bin::binary>>
  end

  defp encode1({:push_u, {:label, name, n}}, labels) when n in 1..32 do
    addr = Map.fetch!(labels, name)
    bin = :binary.encode_unsigned(addr) |> left_pad(n)
    <<0x60 + (n - 1), bin::binary>>
  end

  defp encode1({:jump_label, name}, labels) do
    push_addr(Map.fetch!(labels, name)) <> <<opcode!(:JUMP)>>
  end

  defp encode1({:jumpi_label, name}, labels) do
    push_addr(Map.fetch!(labels, name)) <> <<opcode!(:JUMPI)>>
  end

  defp encode1(list, labels) when is_list(list) do
    IO.iodata_to_binary(Enum.map(list, &encode1(&1, labels)))
  end

  defp encode1(other, _labels), do: raise("Unsupported instruction in pass2: #{inspect(other)}")

  # -------------------------------- helpers ----------------------------------

  defp opcode!(op_atom) do
    case @opcode_map[op_atom] do
      nil -> raise "Unknown opcode: #{inspect(op_atom)}"
      b -> b
    end
  end

  defp int_size(0), do: 0

  defp int_size(int) do
    byte_size(:binary.encode_unsigned(int))
  end

  defp push_addr(addr) when addr <= 255, do: <<0x60, addr>>
  defp push_addr(addr) when addr <= 65_535, do: <<0x61, addr::16>>

  defp push_addr(addr) do
    <<0x63, addr::unsigned-big-integer-size(32)>>
  end

  defp left_pad(bin, n) when is_integer(n) do
    missing = n - byte_size(bin)
    if missing > 0, do: :binary.copy(<<0>>, missing) <> bin, else: bin
  end
end
