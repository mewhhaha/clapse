set positional-arguments := true

default:
  @just --list

clapse-bin:
  #!/usr/bin/env bash
  set -euo pipefail
  mkdir -p artifacts/bin
  include_args=()
  if [[ -s artifacts/latest/clapse_compiler.wasm ]]; then
    include_args+=(--include artifacts/latest/clapse_compiler.wasm)
  fi
  deno compile -A "${include_args[@]}" --output artifacts/bin/clapse scripts/clapse.mjs

compile input output='out/module.wasm': clapse-bin
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" ./artifacts/bin/clapse compile {{input}} {{output}}

format file: clapse-bin
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" ./artifacts/bin/clapse format {{file}}

format-write file: clapse-bin
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" ./artifacts/bin/clapse format --write {{file}}

lsp: clapse-bin
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" ./artifacts/bin/clapse lsp --stdio

formatter-golden-fixtures fixtures='examples/formatter_golden_fixtures.json':
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/formatter-golden-fixtures.mjs --fixtures {{fixtures}} --out out/formatter-golden-fixtures

lsp-wasm-fixtures:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/lsp-wasm-fixtures.mjs

docs-validate:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/validate-docs.mjs

pre-tag-verify:
  deno run -A scripts/guard-no-host-surface.mjs
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}"
  deno run -A scripts/check-pass-manifest.mjs
  deno run -A scripts/record-kernel-smoke.mjs
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" just docs-validate
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" just lsp-wasm-fixtures
  just semantics-check

ci-local:
  #!/usr/bin/env bash
  set -euo pipefail
  just install
  just pre-tag-verify
  just release-candidate out=out/releases-ci-local

browser-compiler-wasm-check wasm='artifacts/latest/clapse_compiler.wasm':
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm {{wasm}}

pass-manifest-check:
  deno run -A scripts/check-pass-manifest.mjs

fib-memo-plugin-smoke:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/fib-memo-plugin-smoke.mjs

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
  mkdir -p artifacts/latest artifacts/bin
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
  if [[ "$compiler_path" != "artifacts/latest/clapse_compiler.wasm" ]]; then
    cp "$compiler_path" artifacts/latest/clapse_compiler.wasm
  fi
  if [[ "${CLAPSE_RUN_WILDCARD_DEMAND_CHECK:-0}" == "1" ]]; then
    just semantics-check
  fi
  deno compile -A --include artifacts/latest/clapse_compiler.wasm --output artifacts/bin/clapse scripts/clapse.mjs
  RUN_HIGHLIGHT_SNAPSHOT_TESTS=1 scripts/setup-helix-local.sh

release-candidate out='out/releases':
  #!/usr/bin/env bash
  set -euo pipefail
  version="$(cat VERSION)"
  commit="$(git rev-parse --short=12 HEAD)"
  release_id="v${version}-${commit}"
  release_dir="{{out}}/${release_id}"
  compiler_wasm="${release_dir}/clapse_compiler.wasm"
  compiler_dts="${release_dir}/clapse_compiler.d.ts"
  compiler_source="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}"
  compiler_source_dts="${compiler_source%.wasm}.d.ts"
  cli_bin="${release_dir}/clapse-bin"
  cli_bin_linux_x64="${release_dir}/clapse-bin-linux-x64"
  cli_bin_linux_arm64="${release_dir}/clapse-bin-linux-arm64"
  cli_bin_macos_x64="${release_dir}/clapse-bin-macos-x64"
  cli_bin_macos_arm64="${release_dir}/clapse-bin-macos-arm64"
  cli_bin_win_x64="${release_dir}/clapse-bin-win-x64.exe"
  behavior_map="${release_dir}/wasm-behavior-fixture-map.json"
  artifact_map="${release_dir}/wasm-selfhost-artifact-fixture-map.json"
  prelude_source="${release_dir}/prelude.clapse"
  mkdir -p "${release_dir}"
  [[ -s "${compiler_source}" ]] || { echo "missing compiler source wasm: ${compiler_source}" >&2; exit 1; }
  [[ -s "${compiler_source_dts}" ]] || { echo "missing compiler source d.ts: ${compiler_source_dts}" >&2; exit 1; }
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "${compiler_source}"
  cp "${compiler_source}" "${compiler_wasm}"
  cp "${compiler_source_dts}" "${compiler_dts}"
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "${compiler_wasm}"
  deno compile -A --output "${cli_bin}" scripts/clapse.mjs
  chmod +x "${cli_bin}"
  deno compile -A --target x86_64-unknown-linux-gnu --output "${cli_bin_linux_x64}" scripts/clapse.mjs
  chmod +x "${cli_bin_linux_x64}"
  deno compile -A --target aarch64-unknown-linux-gnu --output "${cli_bin_linux_arm64}" scripts/clapse.mjs
  chmod +x "${cli_bin_linux_arm64}"
  deno compile -A --target x86_64-apple-darwin --output "${cli_bin_macos_x64}" scripts/clapse.mjs
  chmod +x "${cli_bin_macos_x64}"
  deno compile -A --target aarch64-apple-darwin --output "${cli_bin_macos_arm64}" scripts/clapse.mjs
  chmod +x "${cli_bin_macos_arm64}"
  deno compile -A --target x86_64-pc-windows-msvc --output "${cli_bin_win_x64}" scripts/clapse.mjs
  chmod +x "${cli_bin_win_x64}"
  cp lib/compiler/prelude.clapse "${prelude_source}"
  cp scripts/wasm-behavior-fixture-map.json "${behavior_map}"
  cp scripts/wasm-selfhost-artifact-fixture-map.json "${artifact_map}"
  cli_bin_args=(
    --cli-bin "${cli_bin}"
    --cli-bin "${cli_bin_linux_x64}"
    --cli-bin "${cli_bin_linux_arm64}"
    --cli-bin "${cli_bin_macos_x64}"
    --cli-bin "${cli_bin_macos_arm64}"
    --cli-bin "${cli_bin_win_x64}"
  )
  printf '%s\n' \
    '#!/usr/bin/env bash' \
    'set -euo pipefail' \
    'SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"' \
    'export CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-${SELF_DIR}/clapse_compiler.wasm}"' \
    'exec deno run -A "https://raw.githubusercontent.com/mewhhaha/clapse/${CLAPSE_SCRIPT_REF:-main}/scripts/clapse.mjs" -- "$@"' \
    > "${release_dir}/clapse"
  chmod +x "${release_dir}/clapse"
  deno run -A scripts/release-metadata.mjs --release-id "${release_id}" --clapse-version "${version}" --compiler-wasm "${compiler_wasm}" --compiler-dts "${compiler_dts}" "${cli_bin_args[@]}" --behavior-map "${behavior_map}" --artifact-map "${artifact_map}" --prelude-source "${prelude_source}" --out "${release_dir}/release-manifest.json" --checksums "${release_dir}/checksums.sha256"
  echo "release-candidate: PASS (${release_dir})"
semantics-check:
  just wildcard-demand-check

wildcard-demand-check:
  deno run -A scripts/wildcard-demand-check.mjs
