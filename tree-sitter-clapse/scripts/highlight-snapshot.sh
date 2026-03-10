#!/usr/bin/env bash
set -euo pipefail

grammar_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
fixture_dir="$grammar_dir/test/highlight-fixtures"
query_file="$grammar_dir/queries/highlights.scm"
update=0

for arg in "$@"; do
  case "$arg" in
    --update)
      update=1
      ;;
    --fixtures-dir=*)
      fixture_dir="${arg#*=}"
      ;;
    *)
      echo "usage: $0 [--update] [--fixtures-dir=<path>]" >&2
      exit 2
      ;;
  esac
done

if [[ ! -d "$fixture_dir" ]]; then
  echo "highlight snapshots: fixture dir not found: $fixture_dir" >&2
  exit 1
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
failed=0

cd "$grammar_dir"

while IFS= read -r -d '' fixture; do
  fixture_rel="${fixture#"$grammar_dir"/}"
  expected="${fixture%.clapse}.snap"
  actual="$tmpdir/$(basename "${fixture%.clapse}").snap"

  if ! XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter query --captures "$query_file" "$fixture_rel" >"$actual" 2>"$tmpdir/query.err"; then
    echo "highlight snapshots: query failed for $fixture_rel" >&2
    cat "$tmpdir/query.err" >&2
    failed=1
    continue
  fi

  if [[ "$update" == "1" ]]; then
    cp "$actual" "$expected"
    continue
  fi

  if [[ ! -f "$expected" ]]; then
    echo "highlight snapshots: missing snapshot for $fixture_rel ($expected)" >&2
    failed=1
    continue
  fi

  if ! diff -u "$expected" "$actual" >"$tmpdir/diff.out"; then
    echo "highlight snapshots: mismatch for $fixture_rel" >&2
    cat "$tmpdir/diff.out" >&2
    failed=1
  fi
done < <(find "$fixture_dir" -maxdepth 1 -type f -name '*.clapse' -print0 | sort -z)

if [[ "$failed" != "0" ]]; then
  exit 1
fi

echo "highlight snapshots: PASS"
