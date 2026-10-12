defmodule EVMAssemblerTest do
  use ExUnit.Case, async: false

  alias Logos.Conformance.CorpusRunner
  alias Logos.EVM.Assembler, as: A

  test "opcode-map case lists every atom in @opcode_map" do
    source = File.read!(Path.join(CorpusRunner.root(), "lib/codegen/evm/assembler.ex"))
    [_, map] = String.split(source, "@opcode_map %{\n", parts: 2)
    [map, _] = String.split(map, "\n  }", parts: 2)

    atoms =
      Regex.scan(~r/^\s+([A-Z][A-Z0-9]*):/m, map)
      |> Enum.map(fn [_, atom] -> atom end)

    map_case =
      CorpusRunner.load_cases()
      |> Enum.find(&(&1["id"] == "asm.opcode-map"))

    assert Enum.map(map_case["instructions"], & &1["op"]) == atoms
    assert map_case["class"] == "parity"
  end

  test "push_u 0 is PUSH0 and 256 is PUSH2" do
    assert A.assemble([{:push_u, 0}]) == <<0x5F>>
    assert A.assemble([{:push_u, 256}]) == <<0x61, 0x01, 0x00>>
  end

  test "raw byte immediates are rejected in pass 1" do
    assert_raise RuntimeError, ~r/Unsupported instruction in pass1/, fn ->
      A.assemble([{:op, :PUSH1}, <<1>>])
    end
  end

  @tag timeout: 5_000
  test "jump labels assemble and land on the JUMPDEST" do
    bin =
      A.assemble([
        {:push_u, 1},
        {:jump_label, :L},
        {:label, :L},
        {:op, :STOP}
      ])

    assert bin == <<0x60, 0x01, 0x60, 0x05, 0x56, 0x5B, 0x00>>
  end
end
