defmodule ExpressionPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Logos.Conformance.Watchdog

  @moduletag :property

  @units %{"s" => 1, "m" => 60, "h" => 3_600, "d" => 86_400, "w" => 604_800}

  test "duration units match the documented second multipliers" do
    assert @units["s"] == 1
    assert @units["m"] == 60 * @units["s"]
    assert @units["h"] == 60 * @units["m"]
    assert @units["d"] == 24 * @units["h"]
    assert @units["w"] == 7 * @units["d"]
    assert 7 * @units["d"] == 604_800
  end

  property "comparison duality holds on integers" do
    check all(a <- integer(), b <- integer(), max_runs: 40) do
      assert a < b == not (a >= b)
      assert a > b == not (a <= b)
      assert a == b == not (a != b)
    end
  end

  property "bounded parser input returns a contract or a classified exception" do
    check all(bin <- binary(max_length: 40), max_runs: 20) do
      result = Watchdog.run(fn -> Logos.Parser.parse(bin) end, 500)

      refute match?({:timeout, _}, result)

      assert match?({:ok, %Logos.AST.Contract{}}, result) or match?({:error, _, _}, result)
    end
  end
end
