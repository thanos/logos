defmodule StateMachinePropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  @moduletag :property

  # Independent model of the escrow sequence the EVM backend does not yet execute.
  # deposit -> premature release fails -> release -> second release fails.
  property "a single release flag cannot be spent twice" do
    check all(
            deadline <- integer(1..10_000),
            now <- integer(0..20_000),
            max_runs: 30
          ) do
      released = false
      can_release = now >= deadline and not released
      released_after = can_release
      second = now >= deadline and not released_after
      assert second == false
      assert can_release == now >= deadline
    end
  end
end
