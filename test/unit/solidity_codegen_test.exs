defmodule SolidityCodegenTest do
  use ExUnit.Case, async: true

  alias Logos.Compiler

  test "emits Solidity with clause functions and events" do
    dsl = ~S"""
    Contract C
      State
        record { x : Int = 0 }
      Clauses
        Clause B(): Effect
          Provided That
            x == 0
          Shall
            All [ Set { field: "x", value: 1 }, Emit { event: "Ping", data: {} } ]
    End
    """

    sol = Compiler.compile_to_solidity(dsl)
    assert String.contains?(sol, "contract C")
    assert String.contains?(sol, "function B() external")
    assert String.contains?(sol, "emit Emitted(\"Ping\"")
    assert String.contains?(sol, "x = 1;")
  end
end
