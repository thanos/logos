# A Logos Compiler
This is a compiler for the Logos smart contract language written in Elixir.
It supports both Solidity and EVM bytecode generation.

[Logos Quick Introduction](https://github.com/logos-protocol/logos-compiler/blob/main/docs/quick-introduction.md)

## How To Use

```elixir

# 1) Create a quick Mix project (optional)
mix new legal_compiler && cd legal_compiler
# put the code above into lib/lex_legal_compiler.ex

# 2) Start an IEx session
iex -S mix

# 3) Compile a DSL string
dsl = ~S"""
Contract WorkForHire_M1
  Parties
    client : Party = 0xC1e0000000000000000000000000000000000000
    builder: Party = 0xBu100000000000000000000000000000000000000
    escrow : Party = 0xEsc00000000000000000000000000000000000000

  Definitions
    let price : Decimal = 10
    let currency : Asset<Decimal> = Asset("USDC", 6)

  State
    record {
      submitted : Bool = false,
      accepted  : Bool = false
    }

  Clauses
    Clause SubmitWork() : Effect
      Provided That
        submitted == false
      Shall
        All [
          Set { field: "submitted", value: true },
          Emit { event: "Submitted", data: {} }
        ]

    Clause AcceptWork() : Effect
      Provided That
        submitted == true
      Shall
        All [
          Set { field: "accepted", value: true },
          Emit { event: "Accepted", data: {} }
        ]
      Otherwise
        Remedies
          Emit { event: "AcceptFailed", data: {"reason": "not_submitted"} }

  Term
    until now + Duration("90d")

  GoverningLaw "New York, USA"
  Signatures
    require client signed
    require builder signed
End
"""

sol = Logos.Compiler.compile_to_solidity(dsl)
IO.puts(sol)
```
You’ll get Solidity like:

```Solidity

pragma solidity ^0.8.20;
// Auto-generated from legal DSL // Law: New York, USA
contract WorkForHire_M1 {
  // ---- parties ----
  address public client = 0xC1e0000000000000000000000000000000000000;
  address public builder = 0xBu100000000000000000000000000000000000000;
  address public escrow = 0xEsc00000000000000000000000000000000000000;

  // ---- state ----
  bool public submitted = true;
  bool public accepted = false;

  // ---- events ----
  event Emitted(string ev, string json);
  event TransferNote(string asset, address from_, address to_, uint256 amount);

  // ---- clauses ----
  function SubmitWork() external {
    require(submitted == false, "ProvidedThat failed");
    submitted = true;
    emit Emitted("Submitted", "{}");
  }

  function AcceptWork() external {
    if (!(submitted == true)) {
      emit Emitted("AcceptFailed", "{\"reason\", \"not_submitted\"}");
      return;
    }
    accepted = true;
    emit Emitted("Accepted", "{}");
  }
}

```

For Logos to EVM compiling, you can use the `compile_to_evm` function:

```
dsl = ~S"""
Contract WorkForHire_M1
  Parties
    client : Party = 0xC1e0000000000000000000000000000000000000
    builder: Party = 0xBu100000000000000000000000000000000000000
    escrow : Party = 0xEsc00000000000000000000000000000000000000

  Definitions
    let price : Decimal = 10

  State
    record {
      submitted : Bool = false,
      accepted  : Bool = false
    }

  Clauses
    Clause SubmitWork() : Effect
      Provided That
        submitted == false
      Shall
        All [
          Set { field: "submitted", value: true }
        ]

    Clause AcceptWork() : Effect
      Provided That
        submitted == true
      Shall
        All [
          Set { field: "accepted", value: true }
        ]
      Otherwise
        Remedies
          All [ NoOp ]

  GoverningLaw "New York, USA"
  Signatures
    require client signed
    require builder signed
End
"""

evm = Logos.CompilerEVM.compile_to_evm(dsl)
IO.inspect(evm.runtime_hex, label: "runtime")
IO.inspect(evm.creation_hex, label: "creation")
```

The above  example DSL with Emit will  produce runtime code that logs when a clause fires:

```
dsl = ~S"""
Contract WorkForHire_M1
  Parties
    client : Party = 0xC1e0000000000000000000000000000000000000
    builder: Party = 0xBu100000000000000000000000000000000000000
    escrow : Party = 0xEsc00000000000000000000000000000000000000

  State
    record { submitted : Bool = false, accepted : Bool = false }

  Clauses
    Clause SubmitWork() : Effect
      Provided That
        submitted == false
      Shall
        All [
          Set { field: "submitted", value: true },
          Emit { event: "Submitted", data: {} }
        ]
End
"""

evm = Logos.CompilerEVM.compile_to_evm(dsl)
IO.puts("runtime  = #{String.slice(evm.runtime_hex, 0, 66)}…")
IO.puts("creation = #{String.slice(evm.creation_hex, 0, 66)}…")
```
Events can be filtered with (`ethers.js`):


```javascript
const sig = ethers.id("Emitted(string)");
const ev  = ethers.id("Submitted");
provider.on({
  address: contractAddress,
  topics: [sig, ev]
}, (log) => console.log("Submitted event log:", log));
```
