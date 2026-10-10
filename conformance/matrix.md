# Feature matrix

Status is what the Elixir compiler does at baseline `ae7c13355b50db6d74a1837925e616003108ed79` plus the dirty worktree recorded in `reports/baseline.md`. `BLOCKED` means the case is specified and was not executed because `compile_to_evm/1` raises `Unknown opcode: :SHR`.

| Construct | Parser | AST | Solidity | EVM | Execution |
| --- | --- | --- | --- | --- | --- |
| Contract, clauses, params | accepts | `Contract` / `Clause` | function per clause | raises `:SHR` | BLOCKED |
| State record | accepts | field map | storage declaration | latent | BLOCKED |
| Provided That | raw predicate string | raw string | pasted into `require` | latent, comparisons inverted | BLOCKED |
| Set | accepts | `Eff` `:set` | assignment | latent | Solidity differential sets `x` to 1 |
| Emit | accepts | `Eff` `:emit` | `Emitted` event | latent, SHA3 event id | BLOCKED for Logos bytecode |
| All | nested list kept as one raw effect when the inner form is not parsed | see `KF-PARSE-NESTED-ALL` | sequential statements when parsed | latent | BLOCKED |
| Transfer | accepts | `Eff` `:transfer` | `TransferNote`, not `transferFrom` | latent | BLOCKED |
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
| Duration, now | raw predicate text | raw | pasted through | `sec/2` multipliers exist and are unreached | BLOCKED |
| Type checking | no checker | names stay strings | n/a | unknown names become `PUSH 0` once assembly runs | `SPEC_GAP` |
| Assembler `assemble/1` | n/a | n/a | n/a | PUSH and opcodes work; jumps hang in `left_pad/2` | hand-built `PUSH0` creation deploys |
| ERC-20 doubles | n/a | n/a | compiled with solc, not Logos | n/a | executed when solc and Anvil are installed |
