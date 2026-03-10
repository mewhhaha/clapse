#!/usr/bin/env bash
set -euo pipefail

grammar_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repo_root="$(cd "$grammar_dir/.." && pwd)"
query_file="$grammar_dir/queries/highlights.scm"
manifest_file="$grammar_dir/test/highlight-expectations.txt"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
config_file="$tmpdir/tree-sitter-config.json"

cat > "$config_file" <<EOF
{
  "parser-directories": ["$grammar_dir"]
}
EOF

if [[ ! -f "$manifest_file" ]]; then
  echo "highlight-expectations: missing manifest: $manifest_file" >&2
  exit 1
fi

failed=0
checked=0

while IFS='|' read -r rel_path checks; do
  [[ -z "${rel_path// }" ]] && continue
  [[ "${rel_path:0:1}" == "#" ]] && continue

  source_file="$repo_root/$rel_path"
  if [[ ! -f "$source_file" ]]; then
    echo "highlight-expectations: missing source: $rel_path" >&2
    failed=$((failed + 1))
    continue
  fi

  safe_name="$(echo "$rel_path" | tr '/.' '__')"
  captures_file="$tmpdir/$safe_name.captures"
  errors_file="$tmpdir/$safe_name.err"

  if ! XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter query --config-path "$config_file" --grammar-path "$grammar_dir" --captures "$query_file" "$source_file" >"$captures_file" 2>"$errors_file"; then
    echo "highlight-expectations: query failed for $rel_path" >&2
    cat "$errors_file" >&2
    failed=$((failed + 1))
    continue
  fi

  IFS=',' read -ra entries <<< "$checks"
  for entry in "${entries[@]}"; do
    entry_trimmed="$(echo "$entry" | xargs)"
    [[ -z "$entry_trimmed" ]] && continue

    token="${entry_trimmed%%=>*}"
    scope="${entry_trimmed#*=>}"
    token="$(echo "$token" | xargs)"
    scope="$(echo "$scope" | xargs)"

    if [[ -z "$token" || -z "$scope" || "$token" == "$scope" ]]; then
      echo "highlight-expectations: invalid entry '$entry_trimmed' for $rel_path (expected token=>scope)" >&2
      failed=$((failed + 1))
      continue
    fi

    if ! awk -v token="$token" -v scope="$scope" '
      index($0, " - " scope ",") > 0 && index($0, "text: `" token "`") > 0 { found = 1 }
      END { exit(found ? 0 : 1) }
    ' "$captures_file"; then
      echo "highlight-expectations: missing token/scope '$token=>$scope' in $rel_path" >&2
      failed=$((failed + 1))
    fi
  done

  checked=$((checked + 1))
done < "$manifest_file"

if (( failed > 0 )); then
  echo "highlight-expectations: FAIL ($failed failures, $checked files checked)" >&2
  exit 1
fi

echo "highlight-expectations: PASS ($checked files)"
