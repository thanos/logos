# Coverage

`mix coveralls.json` exited 0 on seed 253137 (ExCoveralls picks its own seed). 112 tests, 0 failures, 1 nightly test excluded.

The tool total is 78.4% and includes `test/support`. The migration target is line coverage of `lib/`. Relevant lines and misses:

| File | Relevant | Missed | Covered |
| --- | ---: | ---: | ---: |
| `lib/ast.ex` | 0 | 0 | n/a |
| `lib/codegen/evm.ex` | 171 | 67 | 60.8% |
| `lib/codegen/evm/assembler.ex` | 43 | 11 | 74.4% |
| `lib/codegen/solidity.ex` | 66 | 24 | 63.6% |
| `lib/compiler.ex` | 2 | 0 | 100% |
| `lib/logos.ex` | 1 | 0 | 100% |
| `lib/parser.ex` | 171 | 18 | 89.4% |

Relevant lines in `lib/`: 454. Missed: 120. Covered: 334/454 = 73.6%. The 70% target is met. `lib/codegen/evm.ex` stays at 60.8% because `emit/1` raises `Unknown opcode: :SHR` before the later instruction builders run. Those lines stay uncovered until that opcode is added in a separate change.

Credo and Sobelow are not dependencies. This run did not execute them.

## Mutation probes

`scripts/mutation_check.sh` copied the tree to a temp directory, patched the copy, and left this checkout unchanged. Result of that run:

| Probe | Expected | Suite |
| --- | --- | --- |
| Swap `LT` and `GT` in `lib/codegen/evm.ex` | hole | stayed green |
| Drop the `DUP1` after `CALL` in `lib/codegen/evm.ex` | hole | stayed green |
| Change the `asm.jumpi-width` expected size from 6 to 99 | hole | stayed green |
| Drop `return_hex` from the ERC-20 double runner | fail | went red |
| Keccak padding byte `0x01` changed to SHA3 `0x06` | fail | went red |

`compile_to_evm/1` returns bytecode. Selectors and topics are Keccak-256, comparisons push the right-hand value first, and an unknown predicate name is a compile error. Dispatcher `DUP1`, call argument order, and the remaining parser crashes stay `KNOWN_FAILURE`.
