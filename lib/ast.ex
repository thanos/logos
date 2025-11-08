defmodule Logos.AST do
  @moduledoc false
  defmodule Contract,
    do:
      defstruct([
        :name,
        parties: %{},
        defs: %{},
        state: %{},
        clauses: [],
        term: nil,
        law: nil,
        signatures: []
      ])

  # params: list of %{name: "amount", type: :uint256 | :address | :bool}
  defmodule Clause, do: defstruct([:name, params: [], provided: [], shall: nil, remedies: nil])

  defmodule Eff do
    defstruct [:op, :args]
  end
end
