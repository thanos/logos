defmodule ASTTest do
  use ExUnit.Case, async: true

  test "contract and effect structs expose the fields the backends read" do
    contract = %Logos.AST.Contract{name: "C"}
    assert contract.parties == %{}
    assert contract.clauses == []
    assert contract.law == nil

    clause = %Logos.AST.Clause{
      name: "A",
      params: [],
      provided: [],
      shall: %Logos.AST.Eff{op: :noop, args: %{}}
    }

    assert clause.remedies == nil
    assert clause.shall.op == :noop
  end
end
