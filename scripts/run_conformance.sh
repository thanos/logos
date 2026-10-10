#!/usr/bin/env bash
# Full corpus run. NOT_RUN fails this script. `mix test` stays green without Anvil or solc.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

group="all"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --group)
      group="$2"
      shift 2
      ;;
    *)
      echo "unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

mix test --seed 4242

python3 - "$group" << 'PY'
import json, sys
group = sys.argv[1]
report = json.load(open("reports/last-result.json"))
rows = report["results"]

def selected(row):
    if group == "all":
        return True
    if group == "evm":
        return row["id"].startswith("evm.") or row["id"] == "solidity.set-emit-executes"
    if group == "parser":
        return row["id"].startswith("parser.") or row["id"].startswith("lexical.")
    if group == "assembler":
        return row["id"].startswith("asm.") or row["id"].startswith("abi.")
    sys.exit(f"unknown group {group}")

bad = []
for row in rows:
    if not selected(row):
        continue
    if row["status"] == "NOT_RUN":
        reason = (row.get("actual") or {})
        print(f"NOT_RUN {row['id']} {reason}")
        bad.append(row["id"])
    elif row["status"] not in ("PASS", "KNOWN_FAILURE", "BLOCKED", "SKIPPED"):
        print(f"{row['status']} {row['id']}")
        bad.append(row["id"])

if bad:
    sys.exit(1)
print(f"group {group} ok")
PY
