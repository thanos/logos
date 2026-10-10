#!/usr/bin/env bash
# Fail when conformance fixtures drift from conformance/fixtures.sha256.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

python3 - << 'PY'
import hashlib, pathlib, sys
root = pathlib.Path("conformance")
lines = []
for path in sorted(p for p in root.rglob("*") if p.is_file() and p.name != "fixtures.sha256"):
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    lines.append(f"{digest}  {path.as_posix()}")
actual = "\n".join(lines) + "\n"
expected_path = root / "fixtures.sha256"
expected = expected_path.read_text() if expected_path.exists() else ""
if actual != expected:
    sys.stderr.write("fixture hash mismatch. Update conformance/fixtures.sha256 in the same change.\n")
    sys.exit(1)
print(f"fixtures ok ({len(lines)} files)")
PY
