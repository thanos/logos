# Migration readiness

The Rust readiness gate is not met. There is no Rust crate in this tree.

The gate asks for no open P0 SPEC failures in the supported subset, no unbounded hangs, hashed fixtures, and reviewed known failures. This run has 18 `KNOWN_FAILURE` rows and 0 `BLOCKED` rows. Fixture hashes are produced by `scripts/verify_fixtures.sh`. Known-failure owners are blank.

## What compile_to_evm produces

`Logos.Compiler.compile_to_evm/1` returns runtime and creation bytecode. The assembler knows `SHR`, `SHL`, `GAS`, `CALLER`, `LOG3`, `LOG4`, and `RETURN`. `left_pad/2` pads to a width and returns. Jump immediates are the shortest PUSH that reaches the `JUMPDEST`, and pass 1 counts that same width.

Solidity lowers `now` and `Duration("1d12h")`, and a `Transfer` emits `transferFrom` plus `TransferNote`. Emit payloads are deterministic JSON copied with `CODECOPY`.

## Known failures that remain

1. Hash selectors and event signatures with Keccak-256. The compiler uses `:crypto.hash(:sha3_256, ...)`.
2. Push comparison operands in EVM order. The current order inverts `LT` and `GT`.
3. Reject unknown predicate names. They currently become `PUSH 0`.
4. `DUP1` the selector before `EQ` in the dispatcher, check calldata length before the fall-through, pass `CALL` arguments in protocol order, and treat short or false ERC-20 return data as failure.
5. Return a diagnostic for `If`, trailing input, a one-line `GoverningLaw`, and the other parser crashes in `known_failures.json`. Do not discard `Whereas` or `Condition`.

## What already runs

Parser, Solidity, assembler (except jumps), Keccak, ABI, and the independent Anvil ERC-20 doubles run today. The small Set/Emit contract compiles with solc 0.8.28. On Anvil the storage slot is 1, bytecode is 681 bytes, and the call used 46105 gas. Gas and bytecode are recorded and are not required to match a reference. Direct bytecode parity with that Solidity stays blocked on `:SHR`.
