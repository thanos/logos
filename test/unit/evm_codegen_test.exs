defmodule EVMCodegenTest do
  use ExUnit.Case, async: true

  alias Logos.Compiler

  @dsl ~S"""
  Contract Escrow
    Parties
      payer  : Party = 0x1111111111111111111111111111111111111111
      payee  : Party = 0x2222222222222222222222222222222222222222
      token  : Party = 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48

    State
      record {
        submitted : Bool = false,
        start_ts  : Int  = 1700000000
      }

    Clauses
      Clause Submit(): Effect
        Provided That
          submitted == false
        Shall
          Set { field: "submitted", value: true }

      Clause Release(to: Address, amount: Int): Effect
        Provided That
          submitted == true and now >= start_ts + 7d
        Shall
          All [
            Transfer { asset: token, from: payer, to: to, amount: amount }
          ]
  End
  """

  test "compile_to_evm raises Unknown opcode :SHR before producing bytecode" do
    assert_raise RuntimeError, "Unknown opcode: :SHR", fn ->
      Compiler.compile_to_evm(@dsl)
    end
  end
end
