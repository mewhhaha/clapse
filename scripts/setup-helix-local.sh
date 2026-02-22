#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
grammar_source_dir="$repo_root/tree-sitter-clapse"
languages_toml="$repo_root/.helix/languages.toml"
helix_config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/helix"
runtime_dir="$helix_config_dir/runtime"
queries_dir="$runtime_dir/queries/clapse"

require_cmd() {
  local cmd="$1"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "missing required command: $cmd" >&2
    exit 1
  fi
}

sync_grammar_source_path() {
  local tmp_file
  tmp_file="$(mktemp)"

  awk -v grammar_path="$grammar_source_dir" '
    BEGIN {
      in_grammar = 0
      is_clapse = 0
      updated = 0
    }
    $0 == "[[grammar]]" {
      in_grammar = 1
      is_clapse = 0
    }
    in_grammar && $0 == "name = \"clapse\"" {
      is_clapse = 1
    }
    in_grammar && is_clapse && $0 ~ /^source = \{ path = "/ {
      $0 = "source = { path = \"" grammar_path "\" }"
      in_grammar = 0
      is_clapse = 0
      updated = 1
    }
    { print }
    END {
      if (!updated) {
        exit 2
      }
    }
  ' "$languages_toml" > "$tmp_file"

  mv "$tmp_file" "$languages_toml"
}

require_cmd tree-sitter
require_cmd hx

sync_grammar_source_path

(
  cd "$grammar_source_dir"
  tree-sitter generate
  XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter test
)

mkdir -p "$runtime_dir/grammars" "$queries_dir"

for query_file in "$grammar_source_dir"/queries/*.scm; do
  cp "$query_file" "$queries_dir/"
done

(
  cd "$repo_root"
  hx --grammar build clapse
)

echo "Helix clapse grammar installed."
echo "Runtime: $runtime_dir"
echo "Verify: hx --health clapse"
