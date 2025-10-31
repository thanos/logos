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

 ## 5) Worked examples

 ### 5.1 Fixed-price work escrow (classic freelance milestone)

 ```
 Contract WorkForHire_M1
   Parties
     client : Party = 0xC1e...nt
     builder: Party = 0xBu1...dr
     escrow : Party = 0xEsc...rw

   Definitions
     let price : Decimal = 10.0
     let currency : Asset<Decimal> = Asset("USDC", 6)

   Whereas
     assert price > 0

   State
     record {
       submitted : Bool = false,
       accepted  : Bool = false
     }

   Condition SUBMITTED() : Bool =
     submitted = true

   Condition ACCEPTED() : Bool =
     accepted = true

   Clauses
     Clause Deposit() : Effect
       Provided That
         now <= (contract_start + Duration("14d"))
       Shall
         Escrow { asset: currency, agent: escrow, payer: client, payee: builder,
                  amount: price, release_if: "ACCEPTED" }
       Otherwise
         Remedies
           Emit { event: "DepositSkipped", data: {"reason": "past_window"} }

     Clause SubmitWork() : Effect
       Provided That
         not SUBMITTED()
       Shall
         All [
           Set { field: "submitted", value: true },
           Emit { event: "Submitted", data: {} }
         ]

     Clause AcceptWork() : Effect
       Provided That
         SUBMITTED()
       Shall
         All [
           Set { field: "accepted", value: true },
           Transfer { asset: currency, from: escrow, to: builder, amount: price },
           Emit { event: "Accepted", data: {} }
         ]
       Otherwise
         Remedies
           Emit { event: "AcceptFailed", data: {"reason": "not_submitted"} }

   Term
     until contract_start + Duration("90d")

   GoverningLaw "New York, USA"
   Signatures
     require client signed
     require builder signed
 End
```

*Reading like a lawyer:*
* “Provided That … Shall … Otherwise Remedies …” corresponds to conditions, performance, and fallback.
* “Whereas” expresses standing facts.
* “Term until …” expresses expiry.


### 5.2 Simple options contract (call option payout)

```
Contract CashSettledCall
  Parties
    writer : Party = 0xWri...ter
    holder : Party = 0xHol...der
    oracle : Party = 0x0ra...cle

  Definitions
    let strike : Decimal = 2000.0
    let notional : Decimal = 1.0
    let asset : Asset<Decimal> = Asset("ETHUSD", 8)
    let premium : Decimal = 50.0
    let settlement : Time = starts + Duration("30d")

  State
    record {
      premium_paid : Bool = false,
      spot         : Option<Decimal> = None
    }

  Condition PREMIUM_PAID() : Bool = premium_paid

  Clauses
    Clause PayPremium() : Effect
      Provided That
        not PREMIUM_PAID()
        now <= (starts + Duration("3d"))
      Shall
        All [
          Transfer { asset: Asset("USDC", 6), from: holder, to: writer, amount: premium },
          Set { field: "premium_paid", value: true },
          Emit { event: "PremiumPaid", data: {} }
        ]

    Clause PostSpot(quote: Decimal) : Effect
      Provided That
        signer = oracle
        now >= settlement
      Shall
        Set { field: "spot", value: Some(quote) }

    Clause Settle() : Effect
      Provided That
        PREMIUM_PAID()
        spot <> None
        now >= settlement
      Shall
        let s = match spot with { Some x -> x; None -> 0.0 }
        let payoff = max(s - strike, 0.0) * notional
        If { cond: payoff > 0.0,
             then_: Transfer { asset: Asset("USDC", 6), from: writer, to: holder, amount: payoff },
             else_: Emit { event: "ExpiredOutOfMoney", data: {} } }

  Term
    until settlement + Duration("7d")

  GoverningLaw "English Law"
  Signatures
    require writer signed
    require holder signed
End
```

## 6) Why this works well (law + code)
* *Clarity for counsel*: “Whereas / Provided That / Shall / Remedies / Term” maps 1:1 to traditional drafting.
* *Safety for engineers*: strong types, total functions, effect values instead of side-effects, atomic commits.
* *: each clause returns a single effect tree that can be inspected, simulated, and formally checked.
* *Determinism*: no randomness, no uncontrolled I/O; oracle inputs are explicit parameters or signed posts.
