defmodule EVMAssemblerTest do
  use ExUnit.Case, async: false

  alias Logos.Conformance.Watchdog
  alias Logos.EVM.Assembler, as: A

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
  test "jump labels are killed by the watchdog instead of hanging the suite" do
    assert {:timeout, 800} =
             Watchdog.run(
               fn ->
                 A.assemble([
                   {:push_u, 1},
                   {:jump_label, :L},
                   {:label, :L},
                   {:op, :STOP}
                 ])
               end,
               800
             )
  end
end
