#!/usr/bin/env bash
set -euo pipefail

grammar_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repo_root="$(cd "$grammar_dir/.." && pwd)"
query_file="$grammar_dir/queries/highlights.scm"
manifest_file="$grammar_dir/test/highlight-real-sources.txt"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
config_file="$tmpdir/tree-sitter-config.json"

cat > "$config_file" <<EOF
{
  "parser-directories": ["$grammar_dir"]
}
EOF

if [[ ! -f "$manifest_file" ]]; then
  echo "highlight-real-sources-smoke: missing manifest: $manifest_file" >&2
  exit 1
fi

failed=0
checked=0

while IFS='|' read -r rel_path min_captures required_scopes; do
  [[ -z "${rel_path// }" ]] && continue
  [[ "${rel_path:0:1}" == "#" ]] && continue

  source_file="$repo_root/$rel_path"
  if [[ ! -f "$source_file" ]]; then
    echo "highlight-real-sources-smoke: missing source: $rel_path" >&2
    failed=$((failed + 1))
    continue
  fi

  safe_name="$(echo "$rel_path" | tr '/.' '__')"
  captures_file="$tmpdir/$safe_name.captures"
  errors_file="$tmpdir/$safe_name.err"

  if ! XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter query --config-path "$config_file" --grammar-path "$grammar_dir" --captures "$query_file" "$source_file" >"$captures_file" 2>"$errors_file"; then
    echo "highlight-real-sources-smoke: query failed for $rel_path" >&2
    cat "$errors_file" >&2
    failed=$((failed + 1))
    continue
  fi

  capture_count="$(grep -c "capture:" "$captures_file" || true)"
  if (( capture_count < min_captures )); then
    echo "highlight-real-sources-smoke: too few captures for $rel_path (got $capture_count, expected >= $min_captures)" >&2
    failed=$((failed + 1))
    continue
  fi

  IFS=',' read -ra scopes <<< "$required_scopes"
  for scope in "${scopes[@]}"; do
    scope_trimmed="$(echo "$scope" | xargs)"
    [[ -z "$scope_trimmed" ]] && continue
    if ! grep -q " - $scope_trimmed" "$captures_file"; then
      echo "highlight-real-sources-smoke: missing scope '$scope_trimmed' for $rel_path" >&2
      failed=$((failed + 1))
      continue 2
    fi
  done

  checked=$((checked + 1))
done < "$manifest_file"

if (( failed > 0 )); then
  echo "highlight-real-sources-smoke: FAIL ($failed failures, $checked passed)" >&2
  exit 1
fi

echo "highlight-real-sources-smoke: PASS ($checked files)"
