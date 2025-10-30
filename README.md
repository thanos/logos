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
