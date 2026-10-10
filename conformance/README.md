# Conformance corpus

Cases are JSON. `test/support/corpus_runner.ex` loads them, runs the Elixir compiler, and compares the canonical result with `expect`. When the compiler is wrong, `observed` plus `conformance/known_failures.json` classify the row as `KNOWN_FAILURE` instead of weakening `expect`.

Three classes stay separate:

- `current` locks today's behavior, including bugs.
- `spec` states Ethereum and `docs/quick-introduction.md`. A miss stays red through the registry.
- `parity` is the same JSON, for a future Rust runner. This tree does not contain a Rust crate.

`mix test` is the smoke gate. `scripts/run_conformance.sh` fails when Anvil or solc cases were `NOT_RUN`.
