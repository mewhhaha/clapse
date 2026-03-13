set positional-arguments := true

default:
  @just --list

verify-compiler-fixpoint:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/compiler-fixpoint-check.mjs

clap-bin:
  #!/usr/bin/env bash
  set -euo pipefail
  mkdir -p artifacts/bin
  include_args=()
  if [[ -s artifacts/latest/clap_compiler.wasm ]]; then
    include_args+=(--include artifacts/latest/clap_compiler.wasm)
  fi
  rm -f artifacts/bin/clap
  deno compile -A "${include_args[@]}" --output artifacts/bin/clap scripts/clap.mjs

compile input output='out/module.wasm': clap-bin
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" ./artifacts/bin/clap compile-native {{input}} {{output}}

compile-native input output='out/module.wasm': clap-bin
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" ./artifacts/bin/clap compile-native {{input}} {{output}}

compile-native-debug input output='out/module.wasm' artifacts='out':
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/run-clap-compiler-wasm.mjs compile-native-debug {{input}} {{output}} {{artifacts}}

compile-debug input output='out/module.wasm' artifacts='out':
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/run-clap-compiler-wasm.mjs compile-debug {{input}} {{output}} {{artifacts}}

compile_debug input output='out/module.wasm' artifacts='out':
  just compile-debug {{input}} {{output}} {{artifacts}}

explorer port='36627':
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/explorer.mjs --port {{port}}

bench-rust-compare iterations='2000000' warmup='20000':
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/bench-rust-compare.mjs {{iterations}} {{warmup}}

format file: clap-bin
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" ./artifacts/bin/clap format {{file}}

format-write file: clap-bin
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" ./artifacts/bin/clap format --write {{file}}

lsp: clap-bin
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" ./artifacts/bin/clap lsp --stdio

formatter-golden-fixtures fixtures='examples/formatter_golden_fixtures.json':
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/formatter-golden-fixtures.mjs --fixtures {{fixtures}} --out out/formatter-golden-fixtures

lsp-wasm-fixtures:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/lsp-wasm-fixtures.mjs

docs-validate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/validate-docs.mjs

gen-syntax out='lib/compiler/syntax_cst_generated.clap' grammar='docs/clapse-language/references/grammar.ebnf':
  deno run -A scripts/syntax/gen-cst-from-ebnf.mjs "{{grammar}}" "{{out}}"

gen-syntax-check out='lib/compiler/syntax_cst_generated.clap' grammar='docs/clapse-language/references/grammar.ebnf':
  #!/usr/bin/env bash
  set -euo pipefail
  tmp="$(mktemp "${TMPDIR:-/tmp}/syntax_cst_generated.XXXXXX")"
  deno run -A scripts/syntax/gen-cst-from-ebnf.mjs "{{grammar}}" "$tmp"
  diff -u "$tmp" "{{out}}"
  rm -f "$tmp"

ebnf-tree-sitter-drift-check grammar='docs/clapse-language/references/grammar.ebnf' tree_sitter='tree-sitter-clap/grammar.js':
  deno run -A scripts/syntax/check-ebnf-tree-sitter-drift.mjs "{{grammar}}" "{{tree_sitter}}"

gen-ts-highlights grammar='docs/clapse-language/references/grammar.ebnf' query='tree-sitter-clap/queries/highlights.scm':
  deno run -A scripts/syntax/gen-ts-highlights-from-ebnf.mjs --write "{{grammar}}" "{{query}}"

gen-ts-highlights-check grammar='docs/clapse-language/references/grammar.ebnf' query='tree-sitter-clap/queries/highlights.scm':
  deno run -A scripts/syntax/gen-ts-highlights-from-ebnf.mjs --check "{{grammar}}" "{{query}}"

pre-tag-verify:
  #!/usr/bin/env bash
  set -euo pipefail
  probe_hops="${CLAP_NATIVE_SELFHOST_PROBE_HOPS:-2}"
  just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json
  verify_wasm="${CLAP_COMPILER_WASM_PATH:-artifacts/strict-native/seed.wasm}"
  deno run -A scripts/guard-no-host-surface.mjs
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "${verify_wasm}"
  deno run -A scripts/check-pass-manifest.mjs
  just gen-syntax-check
  just gen-ts-highlights-check
  just ebnf-tree-sitter-drift-check
  CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-strict-producer-check "${verify_wasm}" "${probe_hops}"
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-source-version-propagation-gate "${verify_wasm}" "${probe_hops}"
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just compile-debug-smoke
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-parse-command-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-fold-laws-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-list-fold-fusion-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-raw-boundary-synthesis-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-entrypoint-dce-strict-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-entrypoint-exports-dce-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-ir-liveness-size-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-temp-pruning-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just native-tail-recursion-gate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" deno run -A scripts/record-kernel-smoke.mjs
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just docs-validate
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just lsp-wasm-fixtures
  CLAP_COMPILER_WASM_PATH="${verify_wasm}" just formatter-golden-fixtures
  just semantics-check

browser-compiler-wasm-check wasm='artifacts/latest/clap_compiler.wasm':
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm {{wasm}}

pass-manifest-check:
  deno run -A scripts/check-pass-manifest.mjs

native-compile-smoke:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/compile-native-smoke.mjs

compile-debug-smoke:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/compile-debug-smoke.mjs

native-fold-laws-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-fold-laws-gate.mjs

native-list-fold-fusion-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-list-fold-fusion-gate.mjs

native-raw-boundary-synthesis-gate:
  #!/usr/bin/env bash
  set -euo pipefail
  if [[ -z "${CLAP_COMPILER_WASM_PATH:-}" ]]; then
    just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json
    CLAP_COMPILER_WASM_PATH="artifacts/strict-native/seed.wasm" deno run -A scripts/native-raw-boundary-synthesis-gate.mjs
  else
    CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH}" deno run -A scripts/native-raw-boundary-synthesis-gate.mjs
  fi

native-parse-command-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-parse-command-gate.mjs

native-entrypoint-dce-strict-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-entrypoint-dce-strict-gate.mjs

native-entrypoint-exports-dce-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-entrypoint-exports-dce-gate.mjs

native-program-codegen-semantics-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/strict-native/seed.wasm}" deno run -A scripts/native-program-codegen-semantics-gate.mjs

native-source-owned-optimization-benchmark-gate iterations='20000' warmup='2000' repeats='1' baseline_wasm='':
  #!/usr/bin/env bash
  set -euo pipefail
  args=(--iterations "{{iterations}}" --warmup "{{warmup}}" --repeats "{{repeats}}")
  if [[ -n "{{baseline_wasm}}" ]]; then
    args+=(--baseline-wasm "{{baseline_wasm}}")
  fi
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/source-optimizer-benchmark-gate.mjs "${args[@]}"

native-ir-liveness-size-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-ir-liveness-size-gate.mjs

native-temp-pruning-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-temp-pruning-gate.mjs

native-tail-recursion-gate:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-tail-recursion-gate.mjs

native-bootstrap-seed-smoke wasm='artifacts/latest/clap_compiler.wasm':
  CLAP_COMPILER_WASM_PATH="{{wasm}}" CLAP_USE_WASM_BOOTSTRAP_SEED=1 deno run -A scripts/native-bootstrap-seed-smoke.mjs --wasm {{wasm}}

native-selfhost-probe wasm='artifacts/latest/clap_compiler.wasm' hops='1':
  deno run -A scripts/native-selfhost-probe.mjs --wasm {{wasm}} --hops {{hops}}

native-selfhost-probe-strict wasm='artifacts/latest/clap_compiler.wasm' hops='1':
  CLAP_NATIVE_SELFHOST_FAIL_ON_BOUNDARY_FALLBACK=1 deno run -A scripts/native-selfhost-probe.mjs --wasm {{wasm}} --hops {{hops}} --fail-on-boundary-fallback

native-boundary-strict-smoke:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-boundary-strict-smoke.mjs

native-boundary-strict-smoke-no-fallback:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/native-boundary-strict-smoke.mjs

native-strict-no-fallback-check wasm='artifacts/latest/clap_compiler.wasm' hops='1':
  CLAP_COMPILER_WASM_PATH="{{wasm}}" deno run -A scripts/compile-native-smoke.mjs
  CLAP_COMPILER_WASM_PATH="{{wasm}}" deno run -A scripts/native-boundary-strict-smoke.mjs
  CLAP_NATIVE_SELFHOST_FAIL_ON_BOUNDARY_FALLBACK=1 deno run -A scripts/native-selfhost-probe.mjs --wasm {{wasm}} --hops {{hops}} --fail-on-boundary-fallback

native-strict-producer-check wasm='artifacts/latest/clap_compiler.wasm' hops='1' source_version='':
  #!/usr/bin/env bash
  set -euo pipefail
  wasm_path="{{wasm}}"
  hops="{{hops}}"
  required_source_version="{{source_version}}"
  if [[ -z "$required_source_version" ]]; then
    required_source_version="${CLAP_NATIVE_SOURCE_VERSION_REQUIRED:-}"
  fi
  source_args=()
  if [[ -n "$required_source_version" ]]; then
    source_args+=(--require-source-version "$required_source_version")
  fi
  CLAP_COMPILER_WASM_PATH="$wasm_path" just native-compile-smoke
  CLAP_COMPILER_WASM_PATH="$wasm_path" just native-boundary-strict-smoke
  CLAP_NATIVE_SELFHOST_FAIL_ON_BOUNDARY_FALLBACK=1 deno run -A scripts/native-selfhost-probe.mjs --wasm "$wasm_path" --hops "$hops" --fail-on-boundary-fallback
  CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/native-producer-raw-probe.mjs --wasm "$wasm_path" --hops "$hops" "${source_args[@]}"

native-strict-producer-check-wasm-seed wasm='artifacts/latest/clap_compiler.wasm' hops='1' source_version='':
  #!/usr/bin/env bash
  set -euo pipefail
  wasm_path="{{wasm}}"
  hops="{{hops}}"
  required_source_version="{{source_version}}"
  if [[ -z "$required_source_version" ]]; then
    required_source_version="${CLAP_NATIVE_SOURCE_VERSION_REQUIRED:-}"
  fi
  source_args=()
  if [[ -n "$required_source_version" ]]; then
    source_args+=(--require-source-version "$required_source_version")
  fi
  CLAP_USE_WASM_BOOTSTRAP_SEED=1 CLAP_COMPILER_WASM_PATH="$wasm_path" just native-compile-smoke
  CLAP_USE_WASM_BOOTSTRAP_SEED=1 CLAP_COMPILER_WASM_PATH="$wasm_path" just native-boundary-strict-smoke
  CLAP_USE_WASM_BOOTSTRAP_SEED=1 CLAP_NATIVE_SELFHOST_FAIL_ON_BOUNDARY_FALLBACK=1 deno run -A scripts/native-selfhost-probe.mjs --wasm "$wasm_path" --hops "$hops" --fail-on-boundary-fallback
  CLAP_USE_WASM_BOOTSTRAP_SEED=1 CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/native-producer-raw-probe.mjs --wasm "$wasm_path" --hops "$hops" "${source_args[@]}"

native-strict-producer-check-ts-seed wasm='artifacts/latest/clap_compiler.wasm' hops='1' source_version='':
  #!/usr/bin/env bash
  set -euo pipefail
  just native-strict-producer-check-wasm-seed "{{wasm}}" "{{hops}}" "{{source_version}}"

native-producer-raw-probe wasm='artifacts/latest/clap_compiler.wasm' hops='1' source_version='':
  #!/usr/bin/env bash
  set -euo pipefail
  wasm_path="{{wasm}}"
  hops="{{hops}}"
  required_source_version="{{source_version}}"
  if [[ -z "$required_source_version" ]]; then
    required_source_version="${CLAP_NATIVE_SOURCE_VERSION_REQUIRED:-}"
  fi
  source_args=()
  if [[ -n "$required_source_version" ]]; then
    source_args+=(--require-source-version "$required_source_version")
  fi
  CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/native-producer-raw-probe.mjs --wasm "$wasm_path" --hops "$hops" "${source_args[@]}"

native-producer-payload-scan wasm='artifacts/latest/clap_compiler.wasm' samples='200' source_version='':
  #!/usr/bin/env bash
  set -euo pipefail
  wasm_path="{{wasm}}"
  samples="{{samples}}"
  required_source_version="{{source_version}}"
  if [[ -z "$required_source_version" ]]; then
    required_source_version="${CLAP_NATIVE_SOURCE_VERSION_REQUIRED:-}"
  fi
  source_args=()
  if [[ -n "$required_source_version" ]]; then
    source_args+=(--require-source-version "$required_source_version")
  fi
  CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/native-producer-payload-scan.mjs --wasm "$wasm_path" --samples "$samples" "${source_args[@]}"

native-source-version-propagation-gate wasm='artifacts/latest/clap_compiler.wasm' hops='2' source_version='':
  #!/usr/bin/env bash
  set -euo pipefail
  wasm_path="{{wasm}}"
  hops="{{hops}}"
  required_source_version="{{source_version}}"
  source_args=()
  if [[ -n "$required_source_version" ]]; then
    source_args+=(--source-version "$required_source_version")
  fi
  CLAP_USE_WASM_BOOTSTRAP_SEED=0 CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/native-source-version-propagation-gate.mjs --wasm "$wasm_path" --hops "$hops" "${source_args[@]}"

native-boundary-strict-seed-scan:
  deno run -A scripts/strict-native-seed-scan.mjs

native-boundary-strict-seed-scan-kernel hops='2':
  CLAP_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK=1 deno run -A scripts/strict-native-seed-scan.mjs --no-default-roots --scan-root artifacts --scan-root out --scan-root out=out --require-no-boundary-fallback --kernel-selfhost-hops {{hops}}

bootstrap-native-producer-seed seed='artifacts/strict-native/seed.wasm' out='artifacts/strict-native/native_producer_seed.wasm' meta='artifacts/strict-native/native_producer_seed.meta.json' depth='1' source_version='native-source-2026-03-01-r2':
  deno run -A scripts/build-native-producer-seed.mjs --seed {{seed}} --out {{out}} --meta {{meta}} --depth {{depth}} --source-version {{source_version}}

native-strict-producer-check-no-fallback wasm='artifacts/strict-native/native_producer_seed.wasm' hops='2' source_version='native-source-2026-03-01-r2':
  CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check "{{wasm}}" "{{hops}}" "{{source_version}}"

bootstrap-strict-native-seed out='artifacts/strict-native/seed.wasm' meta='artifacts/strict-native/seed.meta.json':
  #!/usr/bin/env bash
  set -euo pipefail
  out_path="{{out}}"
  meta_path="{{meta}}"
  probe_hops="${CLAP_STRICT_NATIVE_SEED_PROBE_HOPS:-2}"
  required_source_version="${CLAP_NATIVE_SOURCE_VERSION_REQUIRED:-}"
  native_producer_seed_path="${CLAP_BOOTSTRAP_NATIVE_PRODUCER_SEED_PATH:-artifacts/strict-native/native_producer_seed.wasm}"
  bootstrap_seed="${CLAP_BOOTSTRAP_COMPILER_WASM_PATH:-${CLAP_COMPILER_WASM_PATH:-artifacts/strict-native/seed.wasm}}"
  producer_seed_depth="${CLAP_NATIVE_PRODUCER_SEED_DEPTH:-1}"
  promoted_candidate_seed=''
  promoted_candidate_meta=''
  strict_seed_inputs=(
    scripts/native-producer-seed-template.c
    lib/compiler/native_compile.clap
    lib/compiler/native_compile_reachability.clap
  )
  strict_check_args=("$out_path" "$probe_hops")
  propagation_check_args=("$out_path" "$probe_hops")
  producer_seed_args=("$bootstrap_seed" "$out_path" "$meta_path" "$producer_seed_depth")
  if [[ -n "$required_source_version" ]]; then
    strict_check_args+=("$required_source_version")
    propagation_check_args+=("$required_source_version")
    producer_seed_args+=("$required_source_version")
  fi
  cleanup() {
    rm -f "$promoted_candidate_seed" "$promoted_candidate_meta"
  }
  trap cleanup EXIT
  strict_seed_inputs_fresh=1
  if [[ -s "$out_path" ]]; then
    for seed_input in "${strict_seed_inputs[@]}"; do
      if [[ "$seed_input" -nt "$out_path" ]]; then
        strict_seed_inputs_fresh=0
        break
      fi
    done
  fi
  if [[ -s "$out_path" ]] && [[ "$strict_seed_inputs_fresh" == 1 ]] && CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$out_path" just native-strict-producer-check "${strict_check_args[@]}" >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$out_path" just native-source-version-propagation-gate "${propagation_check_args[@]}" >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$out_path" just native-entrypoint-dce-strict-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$out_path" just native-entrypoint-exports-dce-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$out_path" just native-parse-command-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$out_path" just native-raw-boundary-synthesis-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$out_path" just native-temp-pruning-gate >/dev/null 2>&1; then
    echo "bootstrap-strict-native-seed: retaining existing producer-strict seed at $out_path"
    if [[ ! -s "$meta_path" ]]; then
      mkdir -p "$(dirname "$meta_path")"
      printf '%s\n' \
        '{' \
        "  \"generated_at\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"," \
        '  "tool": "Justfile bootstrap-strict-native-seed",' \
        '  "mode": "native-bootstrap-retain-producer-strict-seed",' \
        "  \"bootstrap\": {\"wasm\": \"$out_path\"}" \
        '}' \
        > "$meta_path"
    fi
  else
    native_seed_check_args=("$native_producer_seed_path" "$probe_hops")
    if [[ -n "$required_source_version" ]]; then
      native_seed_check_args+=("$required_source_version")
    fi
    if [[ -s "$native_producer_seed_path" ]] && CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$native_producer_seed_path" just native-strict-producer-check "${native_seed_check_args[@]}" >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$native_producer_seed_path" just native-source-version-propagation-gate "${native_seed_check_args[@]}" >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$native_producer_seed_path" just native-entrypoint-dce-strict-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$native_producer_seed_path" just native-entrypoint-exports-dce-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$native_producer_seed_path" just native-parse-command-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$native_producer_seed_path" just native-raw-boundary-synthesis-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$native_producer_seed_path" just native-temp-pruning-gate >/dev/null 2>&1; then
      mkdir -p "$(dirname "$out_path")"
      if [[ "$native_producer_seed_path" != "$out_path" ]]; then
        cp "$native_producer_seed_path" "$out_path"
      fi
      native_seed_dts="${native_producer_seed_path%.wasm}.d.ts"
      out_dts="${out_path%.wasm}.d.ts"
      if [[ -s "$native_seed_dts" ]]; then
        if [[ "$native_seed_dts" != "$out_dts" ]]; then
          cp "$native_seed_dts" "$out_dts"
        fi
      else
        printf '%s\n' \
          'export declare function clap_run(request_handle: number): number;' \
          'export declare function main(arg0: number): number;' \
          > "$out_dts"
      fi
      mkdir -p "$(dirname "$meta_path")"
      printf '%s\n' \
        '{' \
        "  \"generated_at\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"," \
        '  "tool": "Justfile bootstrap-strict-native-seed",' \
        '  "mode": "native-producer-seed-retain",' \
        "  \"bootstrap\": {\"wasm\": \"$native_producer_seed_path\"}" \
        '}' \
        > "$meta_path"
      echo "bootstrap-strict-native-seed: promoted native producer seed artifact to $out_path"
    else
      if [[ -s "$native_producer_seed_path" ]]; then
        promoted_candidate_seed="$(mktemp "${TMPDIR:-/tmp}/clap-promoted-strict-seed.XXXXXX.wasm")"
        promoted_candidate_meta="$(mktemp "${TMPDIR:-/tmp}/clap-promoted-strict-seed.XXXXXX.json")"
        deno run -A scripts/build-native-producer-seed.mjs --seed "$native_producer_seed_path" --out "$promoted_candidate_seed" --meta "$promoted_candidate_meta" --depth "$producer_seed_depth" --source-version "${required_source_version:-native-source-2026-03-01-r2}" >/dev/null 2>&1 || true
        if [[ -s "$promoted_candidate_seed" ]] && CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$promoted_candidate_seed" just native-strict-producer-check "$promoted_candidate_seed" "$probe_hops" "${required_source_version:-native-source-2026-03-01-r2}" >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$promoted_candidate_seed" just native-source-version-propagation-gate "$promoted_candidate_seed" "$probe_hops" "${required_source_version:-native-source-2026-03-01-r2}" >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$promoted_candidate_seed" just native-entrypoint-dce-strict-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$promoted_candidate_seed" just native-entrypoint-exports-dce-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$promoted_candidate_seed" just native-parse-command-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$promoted_candidate_seed" just native-raw-boundary-synthesis-gate >/dev/null 2>&1 && CLAP_COMPILER_WASM_PATH="$promoted_candidate_seed" just native-temp-pruning-gate >/dev/null 2>&1; then
          mkdir -p "$(dirname "$out_path")"
          cp "$promoted_candidate_seed" "$out_path"
          candidate_dts="${native_producer_seed_path%.wasm}.d.ts"
          out_dts="${out_path%.wasm}.d.ts"
          if [[ -s "$candidate_dts" ]]; then
            if [[ "$candidate_dts" != "$out_dts" ]]; then
              cp "$candidate_dts" "$out_dts"
            fi
          else
            printf '%s\n' \
              'export declare function clap_run(request_handle: number): number;' \
              'export declare function main(arg0: number): number;' \
              > "$out_dts"
          fi
          mkdir -p "$(dirname "$meta_path")"
          cp "$promoted_candidate_meta" "$meta_path"
          echo "bootstrap-strict-native-seed: promoted bounded seed candidate derived from native producer seed to $out_path"
        else
          [[ -s "$bootstrap_seed" ]] || { echo "bootstrap-strict-native-seed: bootstrap compiler wasm missing: $bootstrap_seed" >&2; exit 1; }
          just bootstrap-native-producer-seed "${producer_seed_args[@]}"
        fi
      else
        [[ -s "$bootstrap_seed" ]] || { echo "bootstrap-strict-native-seed: bootstrap compiler wasm missing: $bootstrap_seed" >&2; exit 1; }
        just bootstrap-native-producer-seed "${producer_seed_args[@]}"
      fi
    fi
  CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$out_path" just native-strict-producer-check "${strict_check_args[@]}"
  CLAP_COMPILER_WASM_PATH="$out_path" just native-source-version-propagation-gate "${propagation_check_args[@]}"
  CLAP_COMPILER_WASM_PATH="$out_path" just native-parse-command-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-raw-boundary-synthesis-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-entrypoint-dce-strict-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-entrypoint-exports-dce-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-temp-pruning-gate
  fi

bootstrap-compiler out='artifacts/latest/clap_compiler.wasm':
  #!/usr/bin/env bash
  set -euo pipefail
  out_path="{{out}}"
  latest_seed_path="${CLAP_BOOTSTRAP_LATEST_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}"
  bootstrap_seed="${CLAP_BOOTSTRAP_COMPILER_WASM_PATH:-${CLAP_COMPILER_WASM_PATH:-}}"
  strict_seed_path="${CLAP_BOOTSTRAP_STRICT_NATIVE_SEED_PATH:-artifacts/strict-native/seed.wasm}"
  if [[ -z "$bootstrap_seed" ]]; then
    if [[ -s "$strict_seed_path" ]]; then
      bootstrap_seed="$strict_seed_path"
    elif [[ -s "$out_path" ]]; then
      bootstrap_seed="$out_path"
    elif [[ -s "$latest_seed_path" ]]; then
      bootstrap_seed="$latest_seed_path"
    else
      echo "bootstrap-compiler: missing non-empty bootstrap compiler wasm; set CLAP_BOOTSTRAP_COMPILER_WASM_PATH/CLAP_COMPILER_WASM_PATH or provide an existing output/latest compiler wasm or strict seed at CLAP_BOOTSTRAP_STRICT_NATIVE_SEED_PATH (${strict_seed_path})" >&2
      exit 1
    fi
  fi
  if [[ ! -s "$bootstrap_seed" ]]; then
    echo "bootstrap-compiler: bootstrap compiler wasm not found or empty: $bootstrap_seed" >&2
    exit 1
  fi
  mkdir -p "$(dirname "$out_path")"
  out_dts="${out_path%.wasm}.d.ts"
  bootstrap_seed_copy=''
  candidate_seed_path="$(mktemp "${TMPDIR:-/tmp}/clap-bootstrap-candidate-seed.XXXXXX.wasm")"
  candidate_seed_meta="$(mktemp "${TMPDIR:-/tmp}/clap-bootstrap-candidate-seed.XXXXXX.json")"
  out_report="$(mktemp "${TMPDIR:-/tmp}/clap-bootstrap-out-report.XXXXXX.json")"
  candidate_report="$(mktemp "${TMPDIR:-/tmp}/clap-bootstrap-candidate-report.XXXXXX.json")"
  cleanup() {
    rm -f "$bootstrap_seed_copy" "$candidate_seed_path" "$candidate_seed_meta" "$out_report" "$candidate_report"
  }
  trap cleanup EXIT
  if [[ "$bootstrap_seed" == "$out_path" ]]; then
    bootstrap_seed_copy="$(mktemp "${TMPDIR:-/tmp}/clap-bootstrap-seed-copy.XXXXXX.wasm")"
    cp "$bootstrap_seed" "$bootstrap_seed_copy"
    bootstrap_seed="$bootstrap_seed_copy"
  fi
  probe_hops="${CLAP_BOOTSTRAP_NATIVE_SELFHOST_PROBE_HOPS:-2}"
  producer_seed_depth="${CLAP_NATIVE_PRODUCER_SEED_DEPTH:-1}"
  max_compiler_bytes="${CLAP_MAX_COMPILER_WASM_BYTES:-67108864}"
  required_source_version="${CLAP_NATIVE_SOURCE_VERSION_REQUIRED:-}"
  strict_check_args=("$probe_hops")
  propagation_check_args=("$probe_hops")
  producer_seed_args=(--seed "$bootstrap_seed" --out "$candidate_seed_path" --meta "$candidate_seed_meta" --depth "$producer_seed_depth")
  if [[ -n "$required_source_version" ]]; then
    strict_check_args+=("$required_source_version")
    propagation_check_args+=("$required_source_version")
    producer_seed_args+=(--source-version "$required_source_version")
  fi
  write_compiler_dts() {
    local path="$1"
    printf '%s\n' \
      'export declare function clap_run(request_handle: number): number;' \
      'export declare function main(arg0: number): number;' \
      > "$path"
  }
  compiler_candidate_within_size() {
    local wasm_path="$1"
    local wasm_bytes
    wasm_bytes="$(wc -c < "$wasm_path")"
    if [[ "$wasm_bytes" -gt "$max_compiler_bytes" ]]; then
      echo "bootstrap-compiler: rejecting oversized compiler candidate at $wasm_path (${wasm_bytes} bytes > ${max_compiler_bytes})" >&2
      return 1
    fi
  }
  validate_compiler_candidate() {
    local wasm_path="$1"
    compiler_candidate_within_size "$wasm_path" &&
      deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "$wasm_path" &&
      CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$wasm_path" just native-strict-producer-check "$wasm_path" "${strict_check_args[@]}" &&
      CLAP_COMPILER_WASM_PATH="$wasm_path" just native-source-version-propagation-gate "$wasm_path" "${propagation_check_args[@]}" &&
      CLAP_COMPILER_WASM_PATH="$wasm_path" just native-entrypoint-dce-strict-gate &&
      CLAP_COMPILER_WASM_PATH="$wasm_path" just native-entrypoint-exports-dce-gate &&
      CLAP_COMPILER_WASM_PATH="$wasm_path" just native-parse-command-gate &&
      CLAP_COMPILER_WASM_PATH="$wasm_path" just native-raw-boundary-synthesis-gate &&
      CLAP_COMPILER_WASM_PATH="$wasm_path" just native-ir-liveness-size-gate &&
      CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$wasm_path" just native-temp-pruning-gate &&
      CLAP_COMPILER_WASM_PATH="$wasm_path" just native-tail-recursion-gate
  }
  write_strategy_report() {
    local wasm_path="$1"
    local report_path="$2"
    CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/selfhost-compile-strategy-report.mjs --out "$report_path" --require-success 1 --require-no-compatibility 1 >/dev/null
  }
  prefer_candidate_seed=0
  compare_reports() {
    local current_report="$1"
    local candidate_report_path="$2"
    deno eval 'const [currentPath, candidatePath] = Deno.args; const current = JSON.parse(await Deno.readTextFile(currentPath)); const candidate = JSON.parse(await Deno.readTextFile(candidatePath)); const score = (report) => { const summary = report?.summary ?? {}; return [Number(summary.ok ?? 0), -Number(summary.failures ?? 0), -Number(summary.compatibility_used ?? 0), Number(summary.compiler_raw ?? 0), -Number(summary.non_raw ?? 0)]; }; const compare = (left, right) => { for (let i = 0; i < left.length; i += 1) { if (left[i] > right[i]) return 1; if (left[i] < right[i]) return -1; } return 0; }; console.log(compare(score(candidate), score(current)) > 0 ? "candidate" : "current");' "$current_report" "$candidate_report_path"
  }
  compile_ok=0
  if CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$bootstrap_seed" deno run -A scripts/run-clap-compiler-wasm.mjs compile-native lib/compiler/kernel.clap "$out_path"; then
    if validate_compiler_candidate "$out_path"; then
      compile_ok=1
      if deno run -A scripts/build-native-producer-seed.mjs "${producer_seed_args[@]}" >/dev/null 2>&1 && validate_compiler_candidate "$candidate_seed_path" && write_strategy_report "$out_path" "$out_report" && write_strategy_report "$candidate_seed_path" "$candidate_report"; then
        if [[ "$(compare_reports "$out_report" "$candidate_report")" == "candidate" ]]; then
          cp "$candidate_seed_path" "$out_path"
          write_compiler_dts "$out_dts"
          prefer_candidate_seed=1
          echo "bootstrap-compiler: promoted fresh native producer seed over kernel self-compile because it has a better public compile floor" >&2
        fi
      fi
    else
      echo "bootstrap-compiler: kernel self-compile produced compiler wasm that failed browser ABI, strict producer checks, source-version propagation checks, or native DCE gates; treating as compile failure" >&2
    fi
  fi
  if [[ "$compile_ok" != "1" ]]; then
    echo "bootstrap-compiler: kernel self-compile failed strict producer/source-version propagation checks or native DCE gates; attempting producer-strict seed retention from bootstrap seed: $bootstrap_seed" >&2
    retention_seed="$bootstrap_seed"
    if [[ -s "$strict_seed_path" ]]; then
      retention_seed="$strict_seed_path"
    fi
    if validate_compiler_candidate "$retention_seed"; then
      if [[ "$retention_seed" != "$out_path" ]]; then
        cp "$retention_seed" "$out_path"
      fi
      seed_dts="${retention_seed%.wasm}.d.ts"
      if [[ -s "$seed_dts" ]]; then
        if [[ "$seed_dts" != "$out_dts" ]]; then
          cp "$seed_dts" "$out_dts"
        fi
      else
        write_compiler_dts "$out_dts"
      fi
      compile_ok=1
      echo "bootstrap-compiler: retained producer-strict bootstrap seed artifact at $out_path from $retention_seed (kernel self-compile result was non-transitive)" >&2
    else
      echo "bootstrap-compiler: kernel self-compile failed from bootstrap seed: $bootstrap_seed" >&2
      exit 1
    fi
  fi
  [[ -s "$out_path" ]] || { echo "bootstrap-compiler: expected output wasm missing: $out_path" >&2; exit 1; }
  [[ -s "$out_dts" ]] || { echo "bootstrap-compiler: expected output d.ts missing: $out_dts" >&2; exit 1; }
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm "$out_path"
  CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$out_path" just native-strict-producer-check "$out_path" "${strict_check_args[@]}"
  CLAP_COMPILER_WASM_PATH="$out_path" just native-source-version-propagation-gate "$out_path" "${propagation_check_args[@]}"
  CLAP_COMPILER_WASM_PATH="$out_path" just native-parse-command-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-raw-boundary-synthesis-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-entrypoint-dce-strict-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-entrypoint-exports-dce-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-ir-liveness-size-gate
  CLAP_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 CLAP_COMPILER_WASM_PATH="$out_path" just native-temp-pruning-gate
  CLAP_COMPILER_WASM_PATH="$out_path" just native-tail-recursion-gate

fib-memo-plugin-smoke:
  CLAP_COMPILER_WASM_PATH="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}" deno run -A scripts/fib-memo-plugin-smoke.mjs

highlights:
  ./tree-sitter-clap/scripts/highlight-snapshot.sh

highlights-update:
  ./tree-sitter-clap/scripts/highlight-snapshot.sh --update

highlights-expect:
  ./tree-sitter-clap/scripts/highlight-expectations.sh

highlights-real:
  ./tree-sitter-clap/scripts/highlight-real-sources-smoke.sh

highlights-helix:
  ./tree-sitter-clap/scripts/highlight-helix-runtime-smoke.sh

install:
  #!/usr/bin/env bash
  set -euo pipefail
  mkdir -p artifacts/latest artifacts/bin
  just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json
  just bootstrap-compiler out/clap_compiler.install.wasm
  cp out/clap_compiler.install.wasm artifacts/latest/clap_compiler.wasm
  cp out/clap_compiler.install.d.ts artifacts/latest/clap_compiler.d.ts
  deno run -A scripts/check-browser-compiler-wasm.mjs --wasm artifacts/latest/clap_compiler.wasm
  if [[ "${CLAP_RUN_WILDCARD_DEMAND_CHECK:-0}" == "1" ]]; then
    just semantics-check
  fi
  rm -f artifacts/bin/clap
  if ! deno compile -A --include artifacts/latest/clap_compiler.wasm --output artifacts/bin/clap scripts/clap.mjs; then
    if [[ -x artifacts/bin/clap ]]; then
      echo "install: warning: deno compile failed; reusing existing artifacts/bin/clap" >&2
    else
      echo "install: warning: deno compile failed; generating deno-run shim artifacts/bin/clap" >&2
      printf '%s\n' \
        '#!/usr/bin/env bash' \
        'set -euo pipefail' \
        'SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"' \
        'REPO_ROOT="$(cd "${SELF_DIR}/../.." && pwd)"' \
        'exec deno run -A "${REPO_ROOT}/scripts/clap.mjs" -- "$@"' \
        > artifacts/bin/clap
      chmod +x artifacts/bin/clap
    fi
  fi
  install_xdg_config_home="${CLAP_INSTALL_XDG_CONFIG_HOME:-${XDG_CONFIG_HOME:-${HOME:-}/.config}}"
  if ! mkdir -p "$install_xdg_config_home" >/dev/null 2>&1 || ! touch "$install_xdg_config_home/.clap_write_test" >/dev/null 2>&1; then
    install_xdg_config_home="$(mktemp -d -t clap-xdg-config-XXXXXX)"
    echo "install: warning: XDG config path not writable; using temporary XDG_CONFIG_HOME=$install_xdg_config_home" >&2
  fi
  rm -f "$install_xdg_config_home/.clap_write_test" >/dev/null 2>&1 || true
  XDG_CONFIG_HOME="$install_xdg_config_home" RUN_HIGHLIGHT_SNAPSHOT_TESTS=1 scripts/setup-helix-local.sh

semantics-check:
  just compile-debug-smoke
  just wildcard-demand-check
  just native-program-codegen-semantics-gate
  just native-source-owned-optimization-benchmark-gate

selfhost-compile-strategy-report manifest='examples/selfhost_behavior_corpus.json' out='out/selfhost-compile-strategy-report.json' mode='debug' require_no_compatibility='0' require_raw_only='0' require_success='0':
  #!/usr/bin/env bash
  set -euo pipefail
  deno run -A scripts/selfhost-compile-strategy-report.mjs \
    --manifest "{{manifest}}" \
    --compile-mode "{{mode}}" \
    --out "{{out}}" \
    --require-no-compatibility "{{require_no_compatibility}}" \
    --require-raw-only "{{require_raw_only}}" \
    --require-success "{{require_success}}"

selfhost-compile-strategy-report-success:
  #!/usr/bin/env bash
  set -euo pipefail
  deno run -A scripts/selfhost-compile-strategy-report.mjs \
    --manifest "examples/selfhost_behavior_corpus.json" \
    --compile-mode "debug" \
    --out "out/selfhost-compile-strategy-report.json" \
    --require-no-compatibility "0" \
    --require-raw-only "0" \
    --require-success "1"

selfhost-compile-strategy-report-raw-only:
  #!/usr/bin/env bash
  set -euo pipefail
  deno run -A scripts/selfhost-compile-strategy-report.mjs \
    --manifest "examples/selfhost_behavior_corpus.json" \
    --compile-mode "debug" \
    --out "out/selfhost-compile-strategy-report.json" \
    --require-no-compatibility "0" \
    --require-raw-only "1" \
    --require-success "1"

full-compiler-verify:
  #!/usr/bin/env bash
  set -euo pipefail
  wasm_path="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}"
  [[ -s "$wasm_path" ]] || { echo "full-compiler-verify: missing compiler wasm at $wasm_path" >&2; exit 1; }
  CLAP_COMPILER_WASM_PATH="$wasm_path" just selfhost-compile-strategy-report-success
  CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/full-compiler-verify.mjs

full-compiler-last-mile-raw-verify:
  #!/usr/bin/env bash
  set -euo pipefail
  wasm_path="${CLAP_COMPILER_WASM_PATH:-artifacts/latest/clap_compiler.wasm}"
  [[ -s "$wasm_path" ]] || { echo "full-compiler-last-mile-raw-verify: missing compiler wasm at $wasm_path" >&2; exit 1; }
  CLAP_COMPILER_WASM_PATH="$wasm_path" deno run -A scripts/full-compiler-last-mile-raw-verify.mjs

wildcard-demand-check:
  deno run -A scripts/wildcard-demand-check.mjs
