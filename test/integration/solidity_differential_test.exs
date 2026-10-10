defmodule SolidityDifferentialTest do
  use ExUnit.Case, async: false

  alias Logos.Conformance.{EVMRunner, Keccak, Tooling}

  @tag timeout: 30_000
  test "Set/Emit stores 1 and emits Emitted" do
    result = EVMRunner.run_scenario("set_emit")

    if Tooling.anvil?() and Tooling.solc?() do
      assert result["status"] == "ok"
      assert result["storage_x"] == "1"
      assert result["log_count"] == "1"
      assert result["topic0"] == "0x" <> Keccak.hash_hex("Emitted(string,string)")
      assert result["gas_used"] =~ ~r/^\d+$/
      assert result["bytecode_bytes"] =~ ~r/^\d+$/
    else
      assert result["status"] == "not_run"
      assert result["reason"] in ["anvil NOT_INSTALLED", "solc NOT_INSTALLED"]
    end
  end
end
