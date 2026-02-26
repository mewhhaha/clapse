#!/usr/bin/env bash
set -euo pipefail

grammar_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repo_root="$(cd "$grammar_dir/.." && pwd)"
helix_config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/helix"
runtime_query="$helix_config_dir/runtime/queries/clapse/highlights.scm"
languages_toml="$helix_config_dir/languages.toml"
source_file="$repo_root/lib/compiler/prelude.clapse"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
config_file="$tmpdir/tree-sitter-config.json"
captures_file="$tmpdir/prelude.captures"
errors_file="$tmpdir/prelude.err"

if [[ ! -s "$runtime_query" ]]; then
  echo "highlight-helix-runtime-smoke: missing runtime query: $runtime_query" >&2
  exit 1
fi

if [[ ! -s "$languages_toml" ]]; then
  echo "highlight-helix-runtime-smoke: missing helix languages config: $languages_toml" >&2
  exit 1
fi

if ! grep -q 'name = "clapse"' "$languages_toml"; then
  echo "highlight-helix-runtime-smoke: clapse language entry missing in $languages_toml" >&2
  exit 1
fi

if ! grep -q 'file-types = \["clapse"\]' "$languages_toml"; then
  echo "highlight-helix-runtime-smoke: clapse file-type mapping missing in $languages_toml" >&2
  exit 1
fi

if rg -q '#set!' "$runtime_query"; then
  echo "highlight-helix-runtime-smoke: runtime query contains #set! directives (unsupported-risk for Helix portability): $runtime_query" >&2
  exit 1
fi

cat > "$config_file" <<EOF
{
  "parser-directories": ["$grammar_dir"]
}
EOF

if ! XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter query --config-path "$config_file" --grammar-path "$grammar_dir" --captures "$runtime_query" "$source_file" >"$captures_file" 2>"$errors_file"; then
  echo "highlight-helix-runtime-smoke: query failed using runtime query file" >&2
  cat "$errors_file" >&2
  exit 1
fi

capture_count="$(grep -c "capture:" "$captures_file" || true)"
if (( capture_count < 300 )); then
  echo "highlight-helix-runtime-smoke: too few runtime captures for prelude.clapse (got $capture_count, expected >= 300)" >&2
  exit 1
fi

for scope in keyword function constant.numeric.integer; do
  if ! grep -q " - $scope," "$captures_file"; then
    echo "highlight-helix-runtime-smoke: missing scope '$scope' in runtime capture output" >&2
    exit 1
  fi
done

health_output="$(hx --health clapse)"

check_health_field() {
  local label="$1"
  local line
  line="$(grep -E "^\\s*${label}:" <<< "$health_output" | tail -n 1 || true)"

  if [[ -z "$line" ]]; then
    echo "highlight-helix-runtime-smoke: hx health missing '$label' check" >&2
    echo "$health_output" >&2
    exit 1
  fi

  if grep -qiE 'error|failed|fail|missing|not found|not configured|disabled|unavailable|✗|✘|×' <<< "$line"; then
    echo "highlight-helix-runtime-smoke: hx health failed $label check" >&2
    echo "$health_output" >&2
    exit 1
  fi
}

check_health_field "Tree-sitter parser"
check_health_field "Highlight queries"

echo "highlight-helix-runtime-smoke: PASS (captures=$capture_count)"
