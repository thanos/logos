## 1) Mental model
* A *Contract* is a module with:
  * *Parties, Definitions, Whereas* statements (facts/invariants),
  * *Clauses* (pure functions over inputs + current state),
  * *Conditions* (“Provided That …” = preconditions),
  * *Performance* (“Shall …” = pure return value describing effects),
  * *Remedies* (alternative effects if conditions fail),
  * *Term* (time box / expiry conditions),
  * *Signatures* (explicit acceptance).
* Everything is *referentially transparent*: clauses compute *effect descriptions* (values) which the runtime later commits atomically on-chain.
* State is immutable within evaluation; changes are expressed as an *Effect* value (e.g., `Transfer`, `Set`, `Escrow`) returned by a clause.

## 2) Core syntax

### 2.1 Lexical
Case-insensitive keywords for legal flavor; identifiers are snake_case.
Statements end with a newline or `;`. Strings in `"…"`. Comments start with `--`.

### 2.2 Types (closed & explicit)

```
Bool, Int, Decimal(scale=18), Time, Duration,
Address, Party, Asset<T>, Map<K,V>, Option<T>, Unit
```
 * `Money = Asset<Decimal>`
 * `now : Time` is provided by the host; deterministic block time or logical time.

### 2.3 Top level

```
Contract <Name>
  Parties
    <id> : Party = <Address>
    ...
  Definitions
    let <name> : <Type> = <expr>
    ...
  Whereas
    assert <predicate_expr>
    ...
  State
    record { <field> : <Type> = <initial_expr>, ... }
  Clauses
    Clause <Name>(<params>) : <Effect>
      Provided That
        <boolean_expr>
        ...
      Shall
        <effect_expr>    -- returned if all Provided That hold
      Otherwise
        Remedies
          <effect_expr>  -- returned if any Provided fails
  Term
    until <time_expr>          -- contract expires
  GoverningLaw "free text"
  Signatures
    require <party_id> signed
    ...
End
```

### 2.4 Expressions (pure, total)

```
<expr> ::=
  literal
| identifier
| let x = <expr> in <expr>
| if <expr> then <expr> else <expr>
| match <expr> with { Some x -> <expr>; None -> <expr> }
| fn(<args>) -> <expr>            -- first-class functions
| <expr> |> <fn>                  -- pipe
| <expr> . <field>                -- record access
| <expr> +|-|*|/ <expr>           -- arithmetic on Int/Decimal
| compare: =, <>, <, <=, >, >=
| logical: and, or, not
```

### 2.5 Effects (first-class values)

```
Effect ::=
  NoOp
| Transfer { asset: Money, from: Party, to: Party, amount: Decimal }
| Escrow   { asset: Money, agent: Party, payer: Party, payee: Party, amount: Decimal, release_if: ConditionId }
| Set      { field: String, value: Any }                -- state update
| Emit     { event: String, data: Map<String, Any> }
| All      [Effect]                                     -- atomic batch
| If       { cond: Bool, then_: Effect, else_: Effect } -- effect-level branching
```

### 2.6 Conditions as named, reusable predicates

```Condition <ConditionId>(<params>) : Bool = <boolean_expr>```

2.7 Grammar (EBNF - not complete)

```
contract   := "Contract" ident nl block "End"
block      := parties defs whereas state clauses term law sigs
parties    := "Parties" nl { ident ":" "Party" "=" address nl }
defs       := "Definitions" nl { "let" ident ":" type "=" expr nl }
whereas    := "Whereas" nl { "assert" expr nl }
state      := "State" nl "record" "{" { ident ":" type "=" expr "," } "}" nl
clauses    := "Clauses" nl { clause }
clause     := "Clause" ident "(" params? ")" ":" "Effect" nl
              "Provided That" nl { expr nl }
              "Shall" nl effect_expr nl
              [ "Otherwise" nl "Remedies" nl effect_expr nl ]
term       := "Term" nl "until" expr nl
law        := "GoverningLaw" string nl
sigs       := "Signatures" nl { "require" ident "signed" nl }
```

## 3) Static semantics

 * *Typing*: Hindley–Milner style with annotations required for top-level `let` and `State` fields.
 * *Purity*: Clauses are total functions `Params × State → Effect`. No I/O; no randomness.
 * *Preconditions*: Every expression under `Provided That` must type-check to `Bool`.
 * *Remedies*: Optional; if omitted and preconditions fail → runtime returns `NoOp` and an error.
 * *Termination*: `Term` guards clause invocation: `now <= term_until`.

 ## 4) Operational model
 1. *Invoke* a clause with parameters + current state.
 2. *Evaluate* pure expressions (deterministic).
 3. If all *Provided* hold → result is `Shall` effect; else → `Remedies` effect (or NoOp).
 4. *Commit* effect atomically (apply all `Set` and `Transfer` in one transaction).
 5. *Emit* events for off-chain observers.
 6. *State updates* are only what `Set` returns.
 This cleanly separates what the lawyery text says from how state changes.
