defmodule ScenarioOracleTest do
  use ExUnit.Case, async: true

  alias Logos.Conformance.CorpusRunner

  test "blocked Logos scenarios record distinct execution outcomes" do
    by_id = Map.new(CorpusRunner.load_cases(), &{&1["id"], &1})

    scenarios =
      for {id, scenario} <- by_id,
          String.starts_with?(id, "evm.scenario-"),
          do: scenario

    assert scenarios != []

    for scenario <- scenarios do
      outcome = scenario["expect"]["execute"]["outcome"]
      assert outcome in ["success", "revert"], scenario["id"]
    end

    premature = by_id["evm.scenario-premature"]["expect"]["execute"]
    deadline = by_id["evm.scenario-deadline"]["expect"]["execute"]
    assert premature["outcome"] == "revert"
    assert premature["storage_unchanged"] == true
    assert deadline["outcome"] == "success"
    assert deadline["storage_unchanged"] == false
    assert premature != deadline

    assert by_id["evm.scenario-escrow-happy"]["expect"]["execute"]["also_blocked_by"] ==
             "escrow_syntax"

    assert by_id["evm.unknown-name-not-zero"]["expect"]["execute"]["forbids"] ==
             "push0_substitution"

    for {id, fragment} <- [
          {"evm.scenario-wrong-caller", "asm.calldata-guard"},
          {"evm.scenario-three-selectors", "asm.dispatch-dup"},
          {"evm.scenario-erc20-malformed", "asm.returndata-width"},
          {"evm.scenario-atomic-all", "asm.call-order"}
        ] do
      assert by_id[id]["expect"]["execute"]["normative_fragment"] == fragment
      frag = by_id[fragment]
      assert frag["class"] == "parity"
      assert frag["implementation_status"] == "passes"
      assert is_binary(frag["expect"]["assemble"]["hex"])
    end
  end

  test "parity cases are shared oracles and do not record a known failure" do
    parity = Enum.filter(CorpusRunner.load_cases(), &(&1["class"] == "parity"))
    assert parity != []
    assert Enum.all?(parity, &is_nil(&1["known_failure_id"]))
    assert Enum.any?(parity, &(&1["id"] == "asm.opcode-map"))
    assert Enum.any?(parity, &(&1["id"] == "asm.jumpdest-at-255")) == false
  end
end
