# Migration contract

Schema version: `1` (`fixture_version` on every case and on `known_failures.json`).

## Phases

`parse | validate | normalize | emit_solidity | emit_evm | execute`

The Elixir runner names the same work `parse`, `solidity`, `evm`, `assemble`, `keccak`, `abi`, `atoms`, and `anvil`. `validate` has no type checker, so those cases are `SKIPPED` with `SPEC_GAP`.

## Canonical comparison

- JSON object keys are sorted.
- Integers are decimal strings.
- Byte strings are `0x` plus lowercase hex.
- Addresses are lowercase hex.
- Diagnostics keep line and column and drop machine-specific paths.
- `false`, `true`, and `null` stay JSON booleans and null.
- Expectation maps match a subset. Extra actual keys are allowed.
- `contains`, `not_contains`, `message_contains`, and `min_growth` are special keys.

A result is `PASS`, `KNOWN_FAILURE`, `BLOCKED`, `SKIPPED`, or `NOT_RUN` in `mix test`. `FAIL`, `TIMEOUT`, and `UNEXPECTED_PASS` fail the suite. Unexecuted work is never `PASS`.

## Golden updates

Changing a fixture requires review. `scripts/verify_fixtures.sh` checks `conformance/fixtures.sha256`. Update that file in the same change. Current Elixir snapshots live in each case's `observed` object. Normative vectors live in `conformance/golden/normative/`.

## Quirks a Rust implementation must not preserve

- Selectors and event ids from NIST SHA3-256.
- Comparison inversion from pushing the left operand and then the right.
- Unknown predicate names compiled as `PUSH 0`.
- Jump placeholders counted as 3 bytes and emitted as 4.
- `left_pad/2` non-termination.
- `Transfer` compiled as an event rather than an ERC-20 call.
- Parser crashes (`UndefinedError`, `MatchError`, `CaseClauseError`, `FunctionClauseError`) in place of a diagnostic.
- Input after `End`, or a missing `End`, accepted by discarding the rest.

## Non-negotiable SPEC

- Keccak-256, not SHA3-256. Empty digest `c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470`.
- ABI words are 32 bytes. `transferFrom(address,address,uint256)` is 4 + 96 = 100 bytes.
- `CALL` stack order is gas, address, value, args offset, args size, ret offset, ret size.
- A failing effect inside `All` reverts the whole call.
- A malformed or empty ERC-20 return is not a silent success. The docs do not define optional-return compatibility; that case stays `SPEC_GAP`.
