#!/usr/bin/env bash
# Print the commit, toolchain, and the exit codes of the baseline commands.
set -u
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

echo "commit $(git rev-parse HEAD)"
echo "dirty $(git status --porcelain | wc -l | tr -d ' ')"
elixir --version
echo "solc $(command -v solc >/dev/null && solc --version | head -1 || echo NOT_INSTALLED)"
echo "anvil $(command -v anvil >/dev/null && anvil --version || echo NOT_INSTALLED)"

set +e
mix deps.get
echo "deps_get_exit:$?"
mix compile --warnings-as-errors
echo "compile_warnings_as_errors_exit:$?"
mix test --seed 4242
echo "mix_test_exit:$?"
set -e
