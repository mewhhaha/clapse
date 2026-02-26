#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
grammar_source_dir="$repo_root/tree-sitter-clapse"
clapse_binary="$repo_root/artifacts/bin/clapse"
languages_toml="$repo_root/.helix/languages.toml"
helix_config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/helix"
runtime_dir="$helix_config_dir/runtime"
queries_dir="$runtime_dir/queries/clapse"
global_languages_toml="$helix_config_dir/languages.toml"

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

ensure_global_clapse_language() {
  mkdir -p "$helix_config_dir"
  if [[ ! -f "$global_languages_toml" ]]; then
    : > "$global_languages_toml"
  fi

  strip_global_clapse_sections "$global_languages_toml"

  cat >> "$global_languages_toml" <<EOF

[[language]]
name = "clapse"
scope = "source.clapse"
file-types = ["clapse"]
comment-token = "--"
language-servers = ["clapse"]
grammar = "clapse"
formatter = { command = "$clapse_binary", args = ["format", "--stdin"] }

[language-server.clapse]
command = "$clapse_binary"
args = ["lsp", "--stdio"]

[[grammar]]
name = "clapse"
source = { path = "$grammar_source_dir" }
EOF
}

strip_global_clapse_sections() {
  local target_file="$1"
  local tmp_file
  tmp_file="$(mktemp)"

  awk '
    function flush_block(emit, i) {
      if (!in_block) {
        return
      }

      emit = 1
      if (block_type == "language" && block_has_clapse_name) {
        emit = 0
      }
      if (block_type == "grammar" && block_has_clapse_name) {
        emit = 0
      }

      if (emit) {
        for (i = 1; i <= block_len; i += 1) {
          print block_lines[i]
        }
      }

      in_block = 0
      block_type = ""
      block_has_clapse_name = 0
      block_len = 0
    }

    {
      line = $0

      if (skip_language_server_clapse) {
        if (line ~ /^\[/) {
          skip_language_server_clapse = 0
        } else {
          next
        }
      }

      if (line ~ /^\[language-server\.clapse(\.|])/ ) {
        flush_block()
        skip_language_server_clapse = 1
        next
      }

      if (line == "[[language]]" || line == "[[grammar]]") {
        flush_block()
        in_block = 1
        block_type = (line == "[[language]]") ? "language" : "grammar"
        block_len = 1
        block_lines[block_len] = line
        next
      }

      if (in_block) {
        if (line ~ /^name[[:space:]]*=[[:space:]]*"clapse"[[:space:]]*$/) {
          block_has_clapse_name = 1
        }

        if (line ~ /^\[/) {
          flush_block()

          if (line ~ /^\[language-server\.clapse(\.|])/ ) {
            skip_language_server_clapse = 1
            next
          }

          if (line == "[[language]]" || line == "[[grammar]]") {
            in_block = 1
            block_type = (line == "[[language]]") ? "language" : "grammar"
            block_len = 1
            block_lines[block_len] = line
            next
          }

          print line
          next
        }

        block_len += 1
        block_lines[block_len] = line
        next
      }

      print line
    }

    END {
      flush_block()
    }
  ' "$target_file" > "$tmp_file"

  mv "$tmp_file" "$target_file"
}

require_cmd tree-sitter
require_cmd hx

if [[ ! -x "$clapse_binary" ]]; then
  echo "missing clapse binary: $clapse_binary (run 'just clapse-bin' or 'just install')" >&2
  exit 1
fi

sync_grammar_source_path
ensure_global_clapse_language

(
  cd "$grammar_source_dir"
  tree-sitter generate
  XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter test
  if [[ "${RUN_HIGHLIGHT_SNAPSHOT_TESTS:-0}" == "1" ]]; then
    "$grammar_source_dir/scripts/highlight-snapshot.sh"
    "$grammar_source_dir/scripts/highlight-expectations.sh"
  fi
)

mkdir -p "$runtime_dir/grammars" "$queries_dir"

for query_file in "$grammar_source_dir"/queries/*.scm; do
  cp "$query_file" "$queries_dir/"
done

(
  cd "$repo_root"
  hx --grammar build clapse
)

if [[ "${RUN_HIGHLIGHT_SNAPSHOT_TESTS:-0}" == "1" ]]; then
  "$grammar_source_dir/scripts/highlight-helix-runtime-smoke.sh"
fi

echo "Helix clapse grammar installed."
echo "Runtime: $runtime_dir"
echo "Verify: hx --health clapse"
