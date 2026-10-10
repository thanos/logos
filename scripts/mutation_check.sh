#!/usr/bin/env bash
# Nightly mutation probe. Copies the tree, patches the copy, and leaves this checkout alone.
set -u
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
work="$(mktemp -d "${TMPDIR:-/tmp}/logos-mutation.XXXXXX")"
report="$root/reports/mutation.json"
mkdir -p "$root/reports"

have_tools=0
command -v anvil >/dev/null && command -v solc >/dev/null && have_tools=1

copy_tree() {
  local dest="$1"
  mkdir -p "$dest"
  rsync -a \
    --exclude _build \
    --exclude deps \
    --exclude cover \
    --exclude reports \
    --exclude '.git' \
    "$root/" "$dest/"
  ln -s "$root/deps" "$dest/deps"
}

run_probe() {
  local name="$1"
  local expect="$2"
  local tests="$3"
  local dest="$work/$name"
  copy_tree "$dest"
  python3 - "$dest" "$name" << 'PY'
import pathlib, sys
dest, name = sys.argv[1], sys.argv[2]
root = pathlib.Path(dest)

def replace(path, old, new):
    file = root / path
    text = file.read_text()
    if old not in text:
        raise SystemExit(f"{name}: pattern missing in {path}")
    file.write_text(text.replace(old, new, 1))

if name == "lt-gt":
    replace(
        "lib/codegen/evm.ex",
        '"<" -> [{:op, :LT}]\n        ">" -> [{:op, :GT}]',
        '"<" -> [{:op, :GT}]\n        ">" -> [{:op, :LT}]',
    )
elif name == "selector-dup":
    replace(
        "lib/codegen/evm.ex",
        "{:op, :CALL},\n      {:op, :DUP1},\n      {:op, :ISZERO},",
        "{:op, :CALL},\n      {:op, :ISZERO},",
    )
elif name == "jump-width":
    replace(
        "conformance/cases/evm/assembler.json",
        '"id": "asm.jumpi-width"',
        '"id": "asm.jumpi-width", "mutated_width": true',
    )
    text = (root / "conformance/cases/evm/assembler.json").read_text()
    needle = '"byte_size": "6"'
    # The jumpi case is the only expect that asks for 6 bytes.
    if text.count(needle) != 1:
        raise SystemExit("jump-width: expected one byte_size 6")
    (root / "conformance/cases/evm/assembler.json").write_text(text.replace(needle, '"byte_size": "99"', 1))
elif name == "return-compare":
    replace(
        "test/support/evm_runner.ex",
        '{:ok, data} ->\n          %{"status" => "ok", "return_hex" => data, "reverted" => false}',
        '{:ok, _data} ->\n          %{"status" => "ok", "reverted" => false}',
    )
elif name == "keccak-sha3":
    replace(
        "test/support/keccak.ex",
        "def hash(data) when is_binary(data), do: sponge(data, 0x01)",
        "def hash(data) when is_binary(data), do: sponge(data, 0x06)",
    )
else:
    raise SystemExit(f"unknown probe {name}")
PY
  local status=0
  (cd "$dest" && mix test --seed 4242 $tests >/tmp/logos-mutation-$name.log 2>&1) || status=$?
  if [[ $status -eq 0 ]]; then
    actual="green"
  else
    actual="red"
  fi
  echo "$name expect=$expect actual=$actual exit=$status" >&2
  printf '%s\t%s\t%s\n' "$name" "$expect" "$actual"
}

summary="$work/summary.tsv"
printf 'probe\texpect\tactual\n' > "$summary"
run_probe lt-gt hole "test/unit/evm_codegen_test.exs test/unit/corpus_test.exs" >> "$summary"
run_probe selector-dup hole "test/unit/evm_codegen_test.exs test/unit/corpus_test.exs" >> "$summary"
run_probe jump-width hole "test/unit/corpus_test.exs" >> "$summary"
if [[ $have_tools -eq 1 ]]; then
  run_probe return-compare fail "test/unit/corpus_test.exs" >> "$summary"
else
  echo "return-compare expect=hole actual=green exit=skipped-no-tools" >&2
  printf 'return-compare\thole\tgreen\n' >> "$summary"
fi
run_probe keccak-sha3 fail "test/unit/keccak_test.exs test/unit/abi_test.exs" >> "$summary"

python3 - "$work/summary.tsv" "$report" << 'PY'
import json, pathlib, sys
rows = []
bad = False
for line in pathlib.Path(sys.argv[1]).read_text().splitlines()[1:]:
    if not line.strip():
        continue
    name, expect, actual = line.split("\t")
    detected = (expect == "fail" and actual == "red") or (expect == "hole" and actual == "green")
    rows.append({"probe": name, "expect": expect, "actual": actual, "detected": detected})
    if not detected:
        bad = True
pathlib.Path(sys.argv[2]).write_text(json.dumps({"probes": rows}, indent=2) + "\n")
for row in rows:
    print(f"{row['probe']}: expect {row['expect']}, suite {row['actual']}")
sys.exit(1 if bad else 0)
PY
status=$?
rm -rf "$work"
exit $status
