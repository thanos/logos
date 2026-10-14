# Logos

Logos is a legal-dialect smart-contract language. This repository is its Elixir compiler. A contract is a set of clauses. Each clause reads parameters and state and returns an effect (`Set`, `Emit`, `Transfer`, or `All`). The compiler lowers that effect to Solidity or to EVM bytecode.

The public API is `Logos.Compiler.compile_to_solidity/1` and `Logos.Compiler.compile_to_evm/1`.

## Contents

1. [Quick introduction](#1-quick-introduction)
2. [Annotated guide of the language](#2-annotated-guide-of-the-language)
3. [Short examples](#3-short-examples)
4. [More complex](#4-more-complex)
5. [Logos to Solidity examples](#5-logos-to-solidity-examples)
6. [BNF of the language](#6-bnf-of-the-language)
7. [Requirements](#7-requirements)
8. [How to build](#8-how-to-build)
9. [Mix commands](#9-mix-commands)
10. [Test the contract on a local Ethereum chain](#10-test-the-contract-on-a-local-ethereum-chain)
11. [Debugging](#11-debugging)
12. [Publishing to the real chain](#12-publishing-to-the-real-chain)

## 1. Quick introduction

A contract names parties, defines values, stores a record, and lists clauses. A clause is a pure function from parameters and state to an effect. The host commits that effect in one transaction.

The words follow a drafting habit:

| Drafting | Logos | Meaning |
| --- | --- | --- |
| Parties | `Parties` | Named addresses |
| Recitals | `Whereas` | Standing facts (kept in the language sketch; the parser currently drops the section) |
| Conditions | `Provided That` | Preconditions |
| Performance | `Shall` | The effect when the preconditions hold |
| Remedies | `Otherwise` / `Remedies` | The effect when they fail |
| Term | `Term` / `until` | Expiry text |
| Governing law | `GoverningLaw` | A comment in the Solidity output |
| Signatures | `Signatures` | Names stored on the AST; not checked on chain |

`now` is block time. `Duration("7d")` and `Duration("1d12h")` are seconds (`604800`, `129600`).

The full language sketch, including forms the compiler does not build yet, is [docs/quick-introduction.md](docs/quick-introduction.md).

## 2. Annotated guide of the language

A contract the current parser accepts looks like this. The notes say what each line becomes.

```
Contract WorkForHire                          -- name of the Solidity contract
  Parties
    client : Party = 0x1111…1111              -- public address; must be 0x hex

  Definitions
    let price : Decimal = 10.50               -- {:decimal, 1050, 2}; type string is kept

  State
    record {
      submitted : Bool = false,               -- storage slot, initial value false
      start_ts  : Int  = 1700000000           -- integer; field names sort into slots
    }

  Clauses
    Clause Pay(to: Address, amount: Int): Effect
      Provided That
        submitted == true and now >= start_ts + Duration("7d")
      Shall
        Transfer { asset: 0xA0b8…eB48, to: to, amount: amount }
      Otherwise
        Remedies
          Emit { event: "PaySkipped", data: { "reason": "early" } }

  Term
    until now + Duration("90d")               -- stored as text; not enforced by itself

  GoverningLaw
    "New York, USA"                           -- header on its own line, then the string

  Signatures
    require client signed
End
```

What the backends do with that text:

- `Provided That` becomes `require` in Solidity, and a compare-then-revert in bytecode. `now` is `block.timestamp` / `TIMESTAMP`. Comparisons push the right-hand value first, so `LT` and `GT` see the left-hand value on top of the stack.
- `Set` writes one storage field.
- `Emit` logs `Emitted`. The data map is deterministic JSON (keys sorted). In bytecode the JSON is copied from the end of the runtime with `CODECOPY`. An address value in the map becomes an extra log topic (`LOG3` or `LOG4`).
- `Transfer` is ERC-20 `transferFrom`. Omitting `from` uses `CALLER` / `msg.sender`. The bytecode also writes a `LOG3`.
- `All [ ... ]` runs the inner effects in order, including a nested `All`.
- Parentheses group arithmetic: `(1 + 2) == 3`.
- `Some(7)` and `None` stay option values. Solidity renders `Some` as its inner value and `None` as `0`.
- `String` and `Bytes` parameters use an ABI head word and then a tail load.
- An unknown name in a predicate, such as `not_a_name`, is a compile error (`unknown name: not_a_name`). `true` and `false` are `1` and `0`.

These forms appear in the language sketch and are not built by this parser yet. `Whereas` and `Condition` are dropped. `Escrow` and `NoOp` become a note emit. `If { ... }` raises. A one-line `GoverningLaw "New York, USA"` is not the `GoverningLaw` header; put the header and the quoted string on separate lines. Party addresses must be hex.

Selectors and event topics are Keccak-256. `Release(address,uint256)` is `f6334794`. `transferFrom(address,address,uint256)` is `23b872dd`.

## 3. Short examples

Save nothing. From the project directory:

```elixir
iex -S mix
```

```elixir
dsl = ~S"""
Contract WorkForHire
  Parties
    client : Party = 0xC1e0000000000000000000000000000000000000

  State
    record { submitted : Bool = false }

  Clauses
    Clause SubmitWork(): Effect
      Provided That
        submitted == false
      Shall
        All [
          Set { field: "submitted", value: true },
          Emit { event: "Submitted", data: {} }
        ]

  GoverningLaw
    "New York, USA"
End
"""

IO.puts(Logos.Compiler.compile_to_solidity(dsl))

evm = Logos.Compiler.compile_to_evm(dsl)
IO.puts(evm.runtime_hex)
IO.puts(evm.creation_hex)
```

`compile_to_evm/1` returns `%{runtime_hex: "0x…", creation_hex: "0x…"}`. `creation_hex` is the bytecode you deploy. `runtime_hex` is the code that remains after the constructor returns.

## 4. More complex

This one adds a decimal definition, a deadline, a payment clause, a remedy, a term, and a signature line.

```
Contract WorkForHire
  Parties
    client : Party = 0x1111111111111111111111111111111111111111

  Definitions
    let price : Decimal = 10.50

  State
    record {
      submitted : Bool = false,
      start_ts : Int = 1700000000
    }

  Clauses
    Clause SubmitWork(): Effect
      Provided That
        submitted == false
      Shall
        All [
          Set { field: "submitted", value: true },
          Emit { event: "Submitted", data: { "note": "paid" } }
        ]

    Clause Pay(to: Address, amount: Int): Effect
      Provided That
        submitted == true and now >= start_ts + Duration("7d")
      Shall
        Transfer {
          asset: 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48,
          from: 0x1111111111111111111111111111111111111111,
          to: to,
          amount: amount
        }
      Otherwise
        Remedies
          Emit { event: "PaySkipped", data: { "reason": "early" } }

  Term
    until now + Duration("90d")

  GoverningLaw
    "New York, USA"

  Signatures
    require client signed
End
```

`Duration("7d")` in the predicate is `604800` seconds. The `Term` line is stored and is not a second predicate unless you repeat it under `Provided That`. `from` is a hex address here so the bytecode pushes that address. Omitting `from` pushes `CALLER`.

## 5. Logos to Solidity examples

`SubmitWork` from the short example becomes:

```solidity
pragma solidity ^0.8.20;
// Auto-generated from legal DSL
// Governing law: New York, USA

contract WorkForHire {
  // ---- parties ----
  address public client = 0xC1e0000000000000000000000000000000000000;

  // ---- state ----
  bool public submitted = false;

  // ---- events ----
  event Emitted(string ev, string json);
  event TransferNote(address asset, address from_, address to_, uint256 amount);

  // ---- clauses ----
  function SubmitWork() external {
    require(submitted == false, "ProvidedThat failed");
        submitted = true;
        emit Emitted("Submitted", "{}");
  }
}
```

A deadline and a transfer lower like this (`Duration("1d12h")` is `129600`):

```solidity
function Release() external {
    require(ready == true && block.timestamp >= 1700000000 + 129600, "ProvidedThat failed");
    (bool ok, ) = 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48.call(
        abi.encodeWithSignature(
            "transferFrom(address,address,uint256)",
            msg.sender,
            0x2222222222222222222222222222222222222222,
            "1050"
        )
    );
    require(ok, "transferFrom");
    emit TransferNote(
        0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48,
        msg.sender,
        0x2222222222222222222222222222222222222222,
        "1050"
    );
}
```

That snippet is what the Solidity backend emits for a literal `amount: 1050`: the amount is quoted text. `solc` will reject it as a `uint256` argument. Use `compile_to_evm/1` when the clause must move tokens. The Set/Emit contract above is the one `solc` accepts.

`solc` must target Shanghai, because the compiler emits `PUSH0`:

```bash
solc --evm-version shanghai --bin WorkForHire.sol
```

## 6. BNF of the language

This is the grammar the parser and the quick introduction share. The longer sketch in [docs/quick-introduction.md](docs/quick-introduction.md) also lists `Whereas`, `Condition`, `Escrow`, `match`, and `let … in`, which this compiler does not build yet.

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
type       := "Int" | "Bool" | "Address" | "Party" | "String" | "Bytes"
            | "Decimal" | "Option" "<" type ">" | "Duration"
params     := param { "," param }
param      := ident ":" type
expr       := term { ("+" | "-") term }
            | expr ("==" | "!=" | "<" | ">" | "<=" | ">=") expr
            | expr ("and" | "or") expr | "not" expr | "(" expr ")"
term       := ident | int | decimal | string | "now" | "Some" "(" expr ")" | "None"
            | "Duration" "(" string ")"
decimal    := int "." int
effect_expr:= "Set" "{" "field" ":" string "," "value" ":" expr "}"
            | "Emit" "{" "event" ":" string "," "data" ":" json "}"
            | "Transfer" "{" "asset" ":" expr "," ["from" ":" expr ","] "to" ":" expr "," "amount" ":" expr "}"
            | "All" "[" effect_expr { "," effect_expr } "]"
            | "If" "{" "cond" ":" expr "," "then_" ":" effect_expr "," "else_" ":" effect_expr "}"
json       := "{" { string ":" expr } "}"
```

`GoverningLaw` in a source file is the header alone, then a quoted string on the next line.

## 7. Requirements

- Elixir `~> 1.18` and a matching Erlang/OTP. Continuous integration uses Elixir 1.18.4 and OTP 28.3.1.
- `mix` from that Elixir install.
- Optional, for chain tests: [Foundry](https://getfoundry.sh) `anvil` and `cast` at v1.8.5, and `solc` 0.8.28. The unit suite stays green when they are absent. `scripts/run_conformance.sh` does not.

## 8. How to build

```bash
git clone https://github.com/thanos/logos.git
cd logos
mix deps.get
mix compile
mix test --seed 4242
```

`mix compile` prints existing warnings in the code generator. The test suite does not treat those warnings as failures.

## 9. Mix commands

| Command | What it does |
| --- | --- |
| `mix deps.get` | Fetch `ex_doc`, and the test-only `excoveralls` and `stream_data` packages |
| `mix compile` | Compile `lib/` |
| `mix test --seed 4242` | Parser, assembler, compiler, and conformance corpus. Seed 4242 matches CI |
| `mix test --only nightly` | The scheduled fuzz tests |
| `mix coveralls` | Coverage in the terminal. `mix coveralls.html` writes `cover/excoveralls.html` |
| `mix docs` | HTML docs from the module documentation |
| `mix format` | Format Elixir sources. CI runs `mix format --check-formatted` |
| `bash scripts/run_conformance.sh` | `mix test`, then fail if an Anvil or `solc` case did not run. `--group evm` runs the chain group |
| `bash scripts/verify_fixtures.sh` | Check `conformance/fixtures.sha256` |
| `bash scripts/collect_baseline.sh` | Print the commit, tool versions, and command status |
| `bash scripts/mutation_check.sh` | Nightly mutation probes |

`iex -S mix` starts a shell with `Logos.Compiler` loaded.

## 10. Test the contract on a local Ethereum chain

Start a Shanghai node so `PUSH0` is active. Leave it running:

```bash
anvil --hardfork shanghai
```

Anvil prints accounts and private keys. The default HTTP endpoint is `http://127.0.0.1:8545`.

In another terminal, from this repository:

```elixir
iex -S mix
```

```elixir
dsl = File.read!("path/to/WorkForHire.logos")
evm = Logos.Compiler.compile_to_evm(dsl)
File.write!("creation.hex", evm.creation_hex)
```

Deploy the creation bytecode. Use one of the keys Anvil printed:

```bash
cast send --rpc-url http://127.0.0.1:8545 \
  --private-key "$ANVIL_KEY" \
  --create "$(cat creation.hex)"
```

`cast` prints the contract address. Call `SubmitWork()`. Its selector is Keccak-256 of `SubmitWork()`, `11844d6e`:

```bash
cast send --rpc-url http://127.0.0.1:8545 \
  --private-key "$ANVIL_KEY" \
  "$CONTRACT" \
  0x11844d6e
```

Read the code back, and read a log:

```bash
cast code --rpc-url http://127.0.0.1:8545 "$CONTRACT"
cast logs --rpc-url http://127.0.0.1:8545 --address "$CONTRACT"
```

The Solidity Set/Emit contract can be deployed the same way after `solc --evm-version shanghai --bin`. Its `Emitted(string,string)` topic is `552ef73d705393dfadd2aef9cf62ca18e26f26c519bd5ce978866e6e944a1c06`. The bytecode backend's first topic is Keccak-256 of `Emitted(string)`, and the second topic is Keccak-256 of the event name.

State-changing calls must be transactions (`cast send` or `eth_sendTransaction`). `eth_call` simulates and does not persist storage.

## 11. Debugging

Compile the contract in `iex` and read the exception. The parser names the bad line (`Bad State field`, `Expected GoverningLaw`, `Term must be until …`). The EVM backend names an unknown predicate (`unknown name: not_a_name`).

Run one test file, or one test, when a change is narrow:

```bash
mix test test/unit/open_issues_test.exs --seed 4242
mix test test/unit/parser_test.exs:42 --seed 4242
```

A conformance failure prints the case id and the status `FAIL`, `UNEXPECTED_PASS`, or `TIMEOUT`. `KNOWN_FAILURE` means the result still matches the recorded defect in `conformance/known_failures.json` and does not fail `mix test`. The case list and the three classes (`current`, `spec`, `parity`) are described in [conformance/README.md](conformance/README.md).

On Anvil, raise the log level and trace a call:

```bash
anvil --hardfork shanghai -vvvv
cast call --rpc-url http://127.0.0.1:8545 --trace "$CONTRACT" 0x11844d6e
```

`cast call` is a dry run. If storage did not change, send a transaction instead.

If `solc` reports `NotActivated`, the bytecode contains a Shanghai opcode and the compiler was invoked without `--evm-version shanghai`.

## 12. Publishing to the real chain

Publishing sends the same creation bytecode to a node you pay for. Point `cast` at that node and use a key that holds ether for gas. Do not commit the key.

```bash
cast send --rpc-url "$RPC_URL" \
  --private-key "$DEPLOY_KEY" \
  --create "$(cat creation.hex)"

cast code --rpc-url "$RPC_URL" "$CONTRACT"
```

The chain must accept Shanghai opcodes (`PUSH0`). After the transaction is mined, `cast code` returns the runtime bytecode. Call clauses with `cast send` the same way as on Anvil.

The generated Solidity can be published with your usual Solidity toolchain. Compile it with `solc --evm-version shanghai` and deploy that artifact. Prefer that path for the Set/Emit contract. Prefer `creation_hex` when the clause is `Transfer`.

`GoverningLaw` and `Signatures` are not enforced by the bytecode. `Term` is not enforced unless the same condition appears under `Provided That`. This compiler is a research artifact: read `conformance/known_failures.json` before treating a deployment as final.
