# Migration readiness

The Rust readiness gate is not met. There is no Rust crate in this tree.

The gate asks for no open P0 SPEC failures in the supported subset, no unbounded hangs, hashed fixtures, and reviewed known failures. This run still has 21 `KNOWN_FAILURE` rows and 14 `BLOCKED` rows. `KF-ASM-LEFT-PAD` is bounded by the watchdog at 800 ms and is still a non-terminating function. Fixture hashes are produced by `scripts/verify_fixtures.sh`. Known-failure owners are blank.

## What blocks execution of Logos bytecode

`Logos.Compiler.compile_to_evm/1` raises `RuntimeError` with message `Unknown opcode: :SHR` from the runtime header in `Logos.Codegen.EVM`. No runtime or creation hex is produced. End-to-end Logos scenarios stay `BLOCKED` on `KF-EVM-UNKNOWN-SHR`.

The assembler also hangs in `left_pad/2` for any `{:jump_label, _}` or `{:jumpi_label, _}` that resolves. `KF-ASM-JUMP-WIDTH` cannot be measured until that hang is gone. Pass 1 adds 3 to the program counter and pass 2 emits `PUSH2`, an address, and `JUMP` or `JUMPI` (4 bytes).

## Minimum compiler patches that would unblock the suite

These are proposed. They are not applied here.

1. Add `:SHR`, `:SHL`, and `:GAS` to the assembler opcode map. `:SHR` is the first raise; the header also emits the other two.
2. Fix `left_pad/2` so the integer clause means "pad to n bytes" and the remaining-count clause is reachable. The first clause currently matches every integer and recurses forever.
3. Count jump placeholders as 4 bytes in pass 1, matching `PUSH2` plus address plus `JUMP`/`JUMPI` in pass 2.
4. Hash selectors and event signatures with Keccak-256. The compiler uses `:crypto.hash(:sha3_256, ...)`.
5. Push comparison operands in EVM order. The current order inverts `LT` and `GT`.
6. Reject unknown predicate names. They currently become `PUSH 0`.
7. `DUP1` the selector before `EQ` in the dispatcher, check calldata length before the fall-through, pass `CALL` arguments in protocol order, and treat short or false ERC-20 return data as failure.
8. Compile `Transfer` as `transferFrom`, not `TransferNote`.
9. Return a diagnostic for `If`, trailing input, a one-line `GoverningLaw`, and the other parser crashes in `known_failures.json`. Do not discard `Whereas` or `Condition`.

After patches 1 and 2, the blocked EVM cases should be re-run. A case that starts passing has to leave the registry; an unexpected pass fails `mix test`.

## What already runs

Parser, Solidity, assembler (except jumps), Keccak, ABI, and the independent Anvil ERC-20 doubles run today. The small Set/Emit contract compiles with solc 0.8.28. On Anvil the storage slot is 1, bytecode is 681 bytes, and the call used 46105 gas. Gas and bytecode are recorded and are not required to match a reference. Direct bytecode parity with that Solidity stays blocked on `:SHR`.
