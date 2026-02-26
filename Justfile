set positional-arguments := true

default:
  @just --list

compiler-path:
  @echo "${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}"

compile input output='out/module.wasm':
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/run-clapse-compiler-wasm.mjs compile {{input}} {{output}}

format file:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/run-clapse-compiler-wasm.mjs format {{file}}

format-write file:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/run-clapse-compiler-wasm.mjs format --write {{file}}

lsp:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/clapse.mjs lsp --stdio

docs-validate:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/validate-docs.mjs

highlights:
  ./tree-sitter-clapse/scripts/highlight-snapshot.sh

highlights-update:
  ./tree-sitter-clapse/scripts/highlight-snapshot.sh --update

highlights-expect:
  ./tree-sitter-clapse/scripts/highlight-expectations.sh

highlights-real:
  ./tree-sitter-clapse/scripts/highlight-real-sources-smoke.sh

highlights-helix:
  ./tree-sitter-clapse/scripts/highlight-helix-runtime-smoke.sh

install:
  #!/usr/bin/env bash
  set -euo pipefail
  mkdir -p artifacts/latest
  compiler_path="${CLAPSE_COMPILER_WASM_PATH:-}"
  if [[ -z "$compiler_path" ]]; then
    if [[ -s artifacts/latest/clapse_compiler.wasm ]]; then
      compiler_path="artifacts/latest/clapse_compiler.wasm"
    elif [[ -s out/clapse_compiler.wasm ]]; then
      compiler_path="out/clapse_compiler.wasm"
    else
      echo "install: missing non-empty compiler wasm; set CLAPSE_COMPILER_WASM_PATH or build out/clapse_compiler.wasm" >&2
      exit 1
    fi
  fi
  if [[ ! -s "$compiler_path" ]]; then
    echo "install: compiler wasm not found or empty: $compiler_path" >&2
    exit 1
  fi
  tmp_compiler="artifacts/latest/clapse_compiler.next.wasm"
  CLAPSE_COMPILER_WASM_PATH="$compiler_path" deno run -A scripts/run-clapse-compiler-wasm.mjs compile lib/compiler/kernel.clapse "$tmp_compiler"
  mv "$tmp_compiler" artifacts/latest/clapse_compiler.wasm
  RUN_HIGHLIGHT_SNAPSHOT_TESTS=1 scripts/setup-helix-local.sh

release-candidate out='out/releases':
  #!/usr/bin/env bash
  set -euo pipefail
  version="$(cat VERSION)"
  commit="$(git rev-parse --short=12 HEAD)"
  release_id="v${version}-${commit}"
  release_dir="{{out}}/${release_id}"
  compiler_wasm="${release_dir}/clapse_compiler.wasm"
  bridge_wasm="${release_dir}/clapse_compiler_bridge.wasm"
  behavior_map="${release_dir}/wasm-behavior-fixture-map.json"
  artifact_map="${release_dir}/wasm-selfhost-artifact-fixture-map.json"
  mkdir -p "${release_dir}"
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/run-clapse-compiler-wasm.mjs compile lib/compiler/kernel.clapse "${compiler_wasm}"
  cp artifacts/latest/clapse_compiler_bridge.wasm "${bridge_wasm}"
  cp scripts/wasm-behavior-fixture-map.json "${behavior_map}"
  cp scripts/wasm-selfhost-artifact-fixture-map.json "${artifact_map}"
  printf '%s\n' \
    '#!/usr/bin/env bash' \
    'set -euo pipefail' \
    'SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"' \
    'export CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-${SELF_DIR}/clapse_compiler.wasm}"' \
    'exec deno run -A "https://raw.githubusercontent.com/mewhhaha/clapse/${CLAPSE_SCRIPT_REF:-main}/scripts/clapse.mjs" --wasm "$@"' \
    > "${release_dir}/clapse"
  chmod +x "${release_dir}/clapse"
  deno run -A scripts/release-metadata.mjs --release-id "${release_id}" --clapse-version "${version}" --compiler-wasm "${compiler_wasm}" --bridge-wasm "${bridge_wasm}" --behavior-map "${behavior_map}" --artifact-map "${artifact_map}" --out "${release_dir}/release-manifest.json" --checksums "${release_dir}/checksums.sha256"
  echo "release-candidate: PASS (${release_dir})"
