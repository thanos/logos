defmodule NightlyFuzzTest do
  use ExUnit.Case, async: false
  use ExUnitProperties

  alias Logos.Conformance.Watchdog

  @moduletag :nightly
  @moduletag timeout: 120_000

  property "longer parser fuzz stays inside the watchdog" do
    check all(bin <- binary(max_length: 80), max_runs: 80) do
      result = Watchdog.run(fn -> Logos.Parser.parse(bin) end, 500)

      refute match?({:timeout, _}, result)

      assert match?({:ok, %Logos.AST.Contract{}}, result) or match?({:error, _, _}, result)
    end
  end
end
