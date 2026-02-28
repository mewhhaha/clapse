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
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" ./artifacts/bin/clapse compile-native {{input}} {{output}}

compile-native input output='out/module.wasm': clapse-bin
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" ./artifacts/bin/clapse compile-native {{input}} {{output}}

compile-native-debug input output='out/module.wasm' artifacts='out':
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/run-clapse-compiler-wasm.mjs compile-native-debug {{input}} {{output}} {{artifacts}}

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
  #!/usr/bin/env bash
  set -euo pipefail
  probe_hops="${CLAPSE_NATIVE_SELFHOST_PROBE_HOPS:-2}"
  just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json
  verify_wasm="${CLAPSE_COMPILER_WASM_PATH:-artifacts/strict-native/seed.wasm}"
  deno run -A scripts/guard-no-host-surface.mjs
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "${verify_wasm}"
  deno run -A scripts/check-pass-manifest.mjs
  CLAPSE_COMPILER_WASM_PATH="${verify_wasm}" just native-compile-smoke
  CLAPSE_COMPILER_WASM_PATH="${verify_wasm}" deno run -A scripts/native-selfhost-probe.mjs --wasm "${verify_wasm}" --hops "${probe_hops}"
  CLAPSE_COMPILER_WASM_PATH="${verify_wasm}" deno run -A scripts/record-kernel-smoke.mjs
  CLAPSE_COMPILER_WASM_PATH="${verify_wasm}" just docs-validate
  CLAPSE_COMPILER_WASM_PATH="${verify_wasm}" just lsp-wasm-fixtures
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

native-compile-smoke:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/compile-native-smoke.mjs

native-selfhost-probe wasm='artifacts/latest/clapse_compiler.wasm' hops='1':
  deno run -A scripts/native-selfhost-probe.mjs --wasm {{wasm}} --hops {{hops}}

native-selfhost-probe-strict wasm='artifacts/latest/clapse_compiler.wasm' hops='1':
  CLAPSE_NATIVE_SELFHOST_FAIL_ON_BOUNDARY_FALLBACK=1 CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0 deno run -A scripts/native-selfhost-probe.mjs --wasm {{wasm}} --hops {{hops}} --fail-on-boundary-fallback

native-boundary-strict-smoke:
  CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/native-boundary-strict-smoke.mjs

native-boundary-strict-smoke-no-fallback:
  CLAPSE_NATIVE_BOUNDARY_FAIL_ON_FALLBACK=1 CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0 CLAPSE_COMPILER_WASM_PATH="${CLAPSE_COMPILER_WASM_PATH:-artifacts/latest/clapse_compiler.wasm}" deno run -A scripts/native-boundary-strict-smoke.mjs

native-strict-no-fallback-check wasm='artifacts/latest/clapse_compiler.wasm' hops='1':
  CLAPSE_NATIVE_COMPILE_SMOKE_FAIL_ON_FALLBACK=1 CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0 CLAPSE_COMPILER_WASM_PATH="{{wasm}}" deno run -A scripts/compile-native-smoke.mjs
  CLAPSE_NATIVE_BOUNDARY_FAIL_ON_FALLBACK=1 CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0 CLAPSE_COMPILER_WASM_PATH="{{wasm}}" deno run -A scripts/native-boundary-strict-smoke.mjs
  CLAPSE_NATIVE_SELFHOST_FAIL_ON_BOUNDARY_FALLBACK=1 CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0 deno run -A scripts/native-selfhost-probe.mjs --wasm {{wasm}} --hops {{hops}} --fail-on-boundary-fallback

native-boundary-strict-seed-scan:
  deno run -A scripts/strict-native-seed-scan.mjs

native-boundary-strict-seed-scan-kernel hops='2':
  CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK=1 deno run -A scripts/strict-native-seed-scan.mjs --no-default-roots --scan-root artifacts --scan-root out --scan-root out=out --require-no-boundary-fallback --kernel-selfhost-hops {{hops}}

bootstrap-strict-native-seed out='artifacts/strict-native/seed.wasm' meta='artifacts/strict-native/seed.meta.json':
  #!/usr/bin/env bash
  set -euo pipefail
  out_path="{{out}}"
  meta_path="{{meta}}"
  probe_hops="${CLAPSE_STRICT_NATIVE_SEED_PROBE_HOPS:-2}"
  if [[ -s "$out_path" ]] && deno run -A scripts/native-selfhost-probe.mjs --wasm "$out_path" --hops "$probe_hops" >/dev/null 2>&1; then
    echo "bootstrap-strict-native-seed: retaining existing selfhost-capable seed at $out_path"
    if [[ ! -s "$meta_path" ]]; then
      mkdir -p "$(dirname "$meta_path")"
      printf '%s\n' \
        '{' \
        "  \"generated_at\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"," \
        '  "tool": "Justfile bootstrap-strict-native-seed",' \
        '  "mode": "native-bootstrap-retain-existing-seed",' \
        "  \"bootstrap\": {\"wasm\": \"$out_path\"}" \
        '}' \
        > "$meta_path"
    fi
  else
    CLAPSE_STRICT_NATIVE_SEED_PROBE_HOPS="$probe_hops" deno run -A scripts/build-strict-native-seed.mjs --out "$out_path" --meta "$meta_path"
  fi

bootstrap-compiler out='artifacts/latest/clapse_compiler.wasm':
  #!/usr/bin/env bash
  set -euo pipefail
  bootstrap_seed="${CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH:-${CLAPSE_COMPILER_WASM_PATH:-}}"
  strict_seed_path="${CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH:-artifacts/strict-native/seed.wasm}"
  if [[ -z "$bootstrap_seed" ]]; then
    if [[ -s "$strict_seed_path" ]]; then
      bootstrap_seed="$strict_seed_path"
    else
      echo "bootstrap-compiler: missing non-empty bootstrap compiler wasm; set CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH/CLAPSE_COMPILER_WASM_PATH or provide strict seed at CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH (${strict_seed_path})" >&2
      exit 1
    fi
  fi
  if [[ ! -s "$bootstrap_seed" ]]; then
    echo "bootstrap-compiler: bootstrap compiler wasm not found or empty: $bootstrap_seed" >&2
    exit 1
  fi
  out_path="{{out}}"
  mkdir -p "$(dirname "$out_path")"
  out_dts="${out_path%.wasm}.d.ts"
  probe_hops="${CLAPSE_BOOTSTRAP_NATIVE_SELFHOST_PROBE_HOPS:-2}"
  compile_ok=0
  if CLAPSE_COMPILER_WASM_PATH="$bootstrap_seed" deno run -A scripts/run-clapse-compiler-wasm.mjs compile-native lib/compiler/kernel.clapse "$out_path"; then
    if deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "$out_path" && deno run -A scripts/native-selfhost-probe.mjs --wasm "$out_path" --hops "$probe_hops"; then
      compile_ok=1
    else
      echo "bootstrap-compiler: kernel self-compile produced compiler wasm that failed browser ABI or self-host probe; treating as compile failure" >&2
    fi
  fi
  if [[ "$compile_ok" != "1" ]]; then
    echo "bootstrap-compiler: kernel self-compile failed self-host probe; attempting native seed retention from bootstrap seed: $bootstrap_seed" >&2
    if deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "$bootstrap_seed" && deno run -A scripts/native-selfhost-probe.mjs --wasm "$bootstrap_seed" --hops "$probe_hops"; then
      if [[ "$bootstrap_seed" != "$out_path" ]]; then
        cp "$bootstrap_seed" "$out_path"
      fi
      seed_dts="${bootstrap_seed%.wasm}.d.ts"
      if [[ -s "$seed_dts" ]]; then
        if [[ "$seed_dts" != "$out_dts" ]]; then
          cp "$seed_dts" "$out_dts"
        fi
      else
        printf '%s\n' \
          'export declare function clapse_run(request_handle: number): number;' \
          'export declare function main(arg0: number): number;' \
          > "$out_dts"
      fi
      compile_ok=1
      echo "bootstrap-compiler: retained bootstrap seed artifact at $out_path (kernel self-compile result was non-transitive)" >&2
    else
      echo "bootstrap-compiler: kernel self-compile failed from bootstrap seed: $bootstrap_seed" >&2
      exit 1
    fi
  fi
  [[ -s "$out_path" ]] || { echo "bootstrap-compiler: expected output wasm missing: $out_path" >&2; exit 1; }
  [[ -s "$out_dts" ]] || { echo "bootstrap-compiler: expected output d.ts missing: $out_dts" >&2; exit 1; }
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "$out_path"
  deno run -A scripts/native-selfhost-probe.mjs --wasm "$out_path" --hops "$probe_hops"
  CLAPSE_COMPILER_WASM_PATH="$out_path" just native-compile-smoke

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
  just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json
  just bootstrap-compiler out/clapse_compiler.install.wasm
  cp out/clapse_compiler.install.wasm artifacts/latest/clapse_compiler.wasm
  cp out/clapse_compiler.install.d.ts artifacts/latest/clapse_compiler.d.ts
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm artifacts/latest/clapse_compiler.wasm
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
  bootstrap_seed="${CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH:-${CLAPSE_COMPILER_WASM_PATH:-}}"
  strict_seed_path="${CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH:-artifacts/strict-native/seed.wasm}"
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
  just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json
  if [[ -z "${bootstrap_seed}" ]]; then
    if [[ -s "${strict_seed_path}" ]]; then
      bootstrap_seed="${strict_seed_path}"
    else
      echo "release-candidate: missing bootstrap compiler wasm; set CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH/CLAPSE_COMPILER_WASM_PATH or provide strict seed at CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH (${strict_seed_path})" >&2
      exit 1
    fi
  fi
  [[ -s "${bootstrap_seed}" ]] || { echo "release-candidate: bootstrap compiler wasm missing: ${bootstrap_seed}" >&2; exit 1; }
  CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH="${bootstrap_seed}" just bootstrap-compiler "${compiler_wasm}"
  [[ -s "${compiler_wasm}" ]] || { echo "release-candidate: compiled compiler wasm missing: ${compiler_wasm}" >&2; exit 1; }
  [[ -s "${compiler_dts}" ]] || { echo "release-candidate: compiled compiler d.ts missing: ${compiler_dts}" >&2; exit 1; }
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
