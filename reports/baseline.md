# Baseline

Recorded from commands run in this worktree on 2026-10-10.

## Commit

`ae7c13355b50db6d74a1837925e616003108ed79`

`git status --porcelain` reported 32 dirty or untracked paths. The conformance harness is part of that dirty tree. Compiler modules were already modified relative to HEAD before the harness was added. SHA-256 of the `lib/` sources that the suite actually compiled:

| File | SHA-256 |
| --- | --- |
| `lib/ast.ex` | `e2e92bf747649ac30986b3ebb2ad7d8df0fb1ba0d2d4b63f79583d0af9040ed5` |
| `lib/compiler.ex` | `54c963a295d48df070933b36ed173fc48823f174e41851213df8e13d8e1596a9` |
| `lib/logos.ex` | `896d77fabfbfc1cd46f8cf77b93f4ffa57c4c36781563c437bff164274012d05` |
| `lib/parser.ex` | `75d96d7120b655f5108d910733f3aeb7e83a2b91eb45729f2da43c56f855bd34` |
| `lib/codegen/evm.ex` | `6cbed3e623212c9e568b72adfe3cfb4613705fc6ce8b020849707f70552a1db4` |
| `lib/codegen/solidity.ex` | `47f0f2c3550a49944e832d105aa74af3de38915dbad81885da257bcdccf9329f` |
| `lib/codegen/evm/assembler.ex` | `54df062cc24c95891bd6ff9f88ddbbb28dfc6ee53e6f0bf0e65c541655162012` |

Every fixture's `baseline_commit` is the HEAD above. The hashes are the dirty compiler the cases were measured against.

## Toolchain

- Elixir 1.18.4, compiled with Erlang/OTP 28 (erts-16.2). `.tool-versions` pins Erlang 28.3.1 and Elixir 1.18.4-otp-28.
- Local Foundry anvil 0.2.0 (`0ed43ce`, 2024-10-11). That version string is not a GitHub release asset, so CI installs Foundry `v1.8.5`. Anvil flags: `--hardfork shanghai` and an ephemeral `--port`. The Erlang port captures stderr. No public RPC.
- solc 0.8.28+commit.7893614a when `solc` is on `PATH`. It is not a project dependency. Fixtures are compiled with `--evm-version shanghai`.
- `excoveralls` 0.18 and `stream_data` 1.1, test-only.

## Commands

`mix compile --warnings-as-errors` exited 1. Warnings, left in the compiler sources:

- `lib/codegen/evm.ex:14` redefines `@moduledoc`.
- `lib/codegen/evm/assembler.ex` unused `code`, unused `pos`, unused `@op`.
- `lib/codegen/solidity.ex:128` single-quoted charlist.

`mix test --seed 4242` exited 0: 1 doctest, 5 properties, 112 tests, 0 failures, 1 excluded (`nightly`). With solc and Anvil on `PATH`, the corpus counts were:

| Status | Count |
| --- | --- |
| PASS | 65 |
| KNOWN_FAILURE | 24 |
| BLOCKED | 14 |
| SKIPPED | 6 |

A later run with solc 0.8.28 and Anvil on `PATH` is the source of these counts. `class` is `parity` on 50 cases, `spec` on 48, and `current` on 11.

`scripts/collect_baseline.sh` prints the commit, versions, and these exit codes.
