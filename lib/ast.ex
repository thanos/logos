defmodule Logos.AST do
  @moduledoc """
  Core abstract syntax tree (AST) and intermediate representations
  for the legal-dialect smart-contract language.

  A `Contract` groups Parties, Definitions, State, Clauses, and optional Term/Law/Signatures.

  Effects are returned by clauses as **values** (no side effects during evaluation)
  and are later **committed atomically** by the host/VM backend.
  """
  defmodule Contract do
    @moduledoc "Top-level contract node."
    @enforce_keys [:name]
    defstruct [
      :name,
      parties: %{},
      defs: %{},
      state: %{},
      types: %{},
      clauses: [],
      term: nil,
      law: nil,
      signatures: []
    ]

    @type t :: %__MODULE__{
            name: String.t(),
            parties: Logos.AST.party_map(),
            defs: Logos.AST.defs_map(),
            state: Logos.AST.state_map(),
            types: map(),
            clauses: [Logos.AST.Clause.t()],
            term: String.t() | nil,
            law: String.t() | nil,
            signatures: [String.t()]
          }
  end

  # params: list of %{name: "amount", type: :uint256 | :address | :bool}
  defmodule Clause do
    @moduledoc """
    A clause is a pure function:

        Params × State → Effect

    `params` keeps ABI-typed metadata so backends can compute selectors and decode inputs.
    """
    @enforce_keys [:name, :params, :provided, :shall]
    defstruct [:name, params: [], provided: [], shall: nil, remedies: nil]

    @type param_type :: :uint256 | :address | :bool | :string | :bytes
    @type param :: %{name: String.t(), type: param_type}
    @type t :: %__MODULE__{
            name: String.t(),
            params: [param],
            provided: [String.t()],
            shall: Logos.AST.Eff.t(),
            remedies: Logos.AST.Eff.t() | nil
          }
  end

  defmodule Eff do
    @moduledoc """
    Effect IR (first-class). Backends lower these to state updates, calls, logs, etc.

    Supported ops:
      * `:noop`
      * `:set`       — `%{ "field" => String.t(), "value" => any }`
      * `:emit`      — `%{ "event" => String.t(), "data" => map | string }` (bytecode logs topics and JSON data)
      * `:all`       — list of child effects
      * `:if`        — `%{ "cond" => String.t(), "then" => Eff, "else" => Eff }`
      * `:transfer`  — ERC-20 transferFrom (pure bytecode backend) with %{asset, from, to, amount}
    """
    @enforce_keys [:op]
    defstruct [:op, :args]

    @type t :: %__MODULE__{op: atom(), args: map() | list() | nil}
  end
end
