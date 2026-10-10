defmodule ParserTest do
  use ExUnit.Case, async: true

  alias Logos.AST
  alias Logos.Parser

  @dsl ~S"""
  Contract WorkForHire_M1
    Parties
      client : Party = 0xC1e0000000000000000000000000000000000000

    Definitions
      let price : Decimal = 10

    State
      record {
        submitted : Bool = false,
        accepted  : Bool = false,
        start_ts  : Int  = 1700000000
      }

    Clauses
      Clause SubmitWork(): Effect
        Provided That
          submitted == false
        Shall
          All [
            Set { field: "submitted", value: true },
            Emit { event: "Submitted", data: {} }
          ]

      Clause Pay(to: Address, amount: Int): Effect
        Provided That
          submitted == true and now >= start_ts + 7d
        Shall
          All [
            Transfer { asset: 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48, from: client, to: to, amount: amount }
          ]
  End
  """

  test "parses contract, state, and clauses" do
    %AST.Contract{name: "WorkForHire_M1", clauses: clauses} = Parser.parse(@dsl)
    assert length(clauses) == 2
    assert Enum.at(clauses, 0).name == "SubmitWork"

    assert Enum.at(clauses, 1).params == [
             %{name: "to", type: :address},
             %{name: "amount", type: :uint256}
           ]
  end
end
