# Feature matrix

Status is what the Elixir compiler does after the open-issue fixes. `compile_to_evm/1` returns bytecode.

| Construct | Parser | AST | Solidity | EVM | Execution |
| --- | --- | --- | --- | --- | --- |
| Contract, clauses, params | accepts | `Contract` / `Clause` | function per clause | runtime and creation bytecode | anvil scenarios compile |
| State record | accepts | field map plus `types` | storage declaration | storage layout | Solidity differential sets `x` to 1 |
| Provided That | raw predicate string | raw string | `now` and durations lowered | comparisons inverted | bytecode produced |
| Set | accepts | `Eff` `:set` | assignment | `SSTORE` | Solidity differential sets `x` to 1 |
| Emit | accepts | `Eff` `:emit` | `Emitted` with JSON data | `CODECOPY` plus `LOG2`/`LOG3`/`LOG4`, SHA3 event id | Solidity differential logs the event |
| All | nested `All` kept | inner op is `all` | sequential statements | sequential effects | bytecode produced |
| Transfer | accepts | `Eff` `:transfer` | `transferFrom` and `TransferNote` | `CALL` plus `LOG3`; omitted `from` is `CALLER` | bytecode produced |
| Escrow, NoOp keyword | rewritten to a Note emit | `KF-PARSE-NOOP-ESCROW` | event | latent | BLOCKED |
| Empty Shall | `:noop` | noop | comment | latent | BLOCKED |
| If | `UndefinedError` | `KF-PARSE-IF` | not reached | not reached | not reached |
| Whereas, Condition | dropped | absent | absent | absent | absent |
| End | optional | trailing text discarded | n/a | n/a | n/a |
| GoverningLaw on its own line | kept | law string | comment | n/a | n/a |
| GoverningLaw on one line with the quoted text | not recognized | `KF-PARSE-LAW-LINE` | n/a | n/a | n/a |
| Signatures | stored, not checked | list of names | not enforced | not enforced | absent |
| match, let-in | absent | absent | absent | absent | absent |
| Term | stored as raw text | raw | not enforced | not enforced | absent |
| Duration, now | raw predicate text | raw | `block.timestamp` and summed seconds, including `1d12h` | seconds via `Logos.Duration` | bytecode produced |
| Type checking | no checker | names stay strings | n/a | unknown names become `PUSH 0` once assembly runs | `SPEC_GAP` |
| Assembler `assemble/1` | n/a | n/a | n/a | every `@opcode_map` byte is a parity case; jumps terminate and use the shortest PUSH that fits | hand-built `PUSH0` creation deploys |
| ERC-20 doubles | n/a | n/a | compiled with solc, not Logos | n/a | true/false/empty/revert/short/long/dirty returns, zero amount, insufficient allowance and balance, EOA call, callback, and the Set/Emit log |
