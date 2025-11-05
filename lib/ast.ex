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

  defmodule Clause, do: defstruct([:name, params: [], provided: [], shall: nil, remedies: nil])
  # Effect IR (still language-agnostic)
  defmodule Eff do
    # op: :set | :emit | :all | :if | :transfer | :noop
    defstruct [:op, :args]
  end
end
