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
