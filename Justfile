set shell := ["bash", "-euo", "pipefail", "-c"]

default:
  @just --list

# Full local DX bootstrap: compiler, formatter/LSP smoke checks, tree-sitter, and Helix wiring.
install:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal build --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary exe:clapse
  deno run -A scripts/build-compiler-wasm-bridge.mjs out/clapse_compiler_bridge.wasm
  printf 'id x = x\n' | CLAPSE_ALLOW_BRIDGE=1 deno run -A scripts/clapse.mjs format --stdin >/dev/null
  CLAPSE_ALLOW_BRIDGE=1 deno run -A scripts/clapse.mjs lsp --stdio </dev/null >/dev/null
  ./scripts/setup-helix-local.sh
  hx --health clapse

grammar:
  cd tree-sitter-clapse && tree-sitter generate
  cd tree-sitter-clapse && XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter test

helix:
  ./scripts/setup-helix-local.sh

deno-tools:
  mkdir -p out/tools
  deno compile -A -o out/tools/clapse scripts/clapse.mjs
  deno compile -A -o out/tools/clapse-run-wasm scripts/run-wasm.mjs
  deno compile -A -o out/tools/clapse-bench-wasm scripts/bench-wasm.mjs

wasm-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_main.clapse out/wasm_main.wasm
  deno run -A scripts/run-wasm.mjs out/wasm_main.wasm main 7

wasm-closure-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_closure.clapse out/wasm_closure.wasm
  deno run -A scripts/run-wasm.mjs out/wasm_closure.wasm main 7

wasm-string-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/strings.clapse out/wasm_strings.wasm
  deno run -A scripts/run-wasm.mjs out/wasm_strings.wasm main

wasm-interop-slice-build:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/interop_slice.clapse out/interop_slice.wasm

wasm-interop-slice-smoke:
  just wasm-interop-slice-build
  output="$(deno run -A examples/interop_slice.mjs out/interop_slice.wasm)"; \
  echo "$output"; \
  test "$(printf '%s\n' "$output" | sed -n '1p')" = "GET 1"; \
  test "$(printf '%s\n' "$output" | sed -n '2p')" = "POST 0"

wasm-runtime-contract-smoke:
  deno run -A scripts/runtime-contract-smoke.mjs

wasm-struct-helpers-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_has_tag.clapse out/wasm_struct_has_tag.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_has_tag_false.clapse out/wasm_struct_has_tag_false.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_get_ok.clapse out/wasm_struct_get_ok.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_get_mismatch.clapse out/wasm_struct_get_mismatch.wasm
  deno run -A scripts/wasm-struct-helpers-smoke.mjs out/wasm_struct_has_tag.wasm out/wasm_struct_has_tag_false.wasm out/wasm_struct_get_ok.wasm out/wasm_struct_get_mismatch.wasm

bootstrap-phase4-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase4_parser_pilot.clapse out/bootstrap_phase4_parser_pilot.wasm
  deno run -A scripts/bootstrap-parser-pilot-smoke.mjs out/bootstrap_phase4_parser_pilot.wasm

bootstrap-phase5-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase5_dispatch_pilot.clapse out/bootstrap_phase5_dispatch_pilot.wasm
  deno run -A scripts/bootstrap-phase5-dispatch-smoke.mjs out/bootstrap_phase5_dispatch_pilot.wasm

bootstrap-phase6-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase6_entry.clapse out/bootstrap_phase6_entry.wasm
  deno run -A scripts/bootstrap-phase6-module-smoke.mjs out/bootstrap_phase6_entry.wasm

bootstrap-phase7-compile:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase7_host_capability_pilot.clapse out/bootstrap_phase7_host_capability_pilot.wasm

bootstrap-phase8-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase8_pattern_and_operators.clapse out/bootstrap_phase8_pattern_and_operators.wasm
  test "$(deno run -A scripts/run-wasm.mjs out/bootstrap_phase8_pattern_and_operators.wasm main 0)" = "0"
  test "$(deno run -A scripts/run-wasm.mjs out/bootstrap_phase8_pattern_and_operators.wasm main 7)" = "7"

bootstrap-phase9-compile out='out/clapse_compiler.wasm':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase9_compiler_kernel.clapse {{out}}

bootstrap-phase9-smoke:
  just bootstrap-phase9-compile out/clapse_compiler.wasm
  test "$(CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler.wasm' deno run -A scripts/run-clapse-compiler-wasm.mjs engine-mode | tail -n 1)" = "wasm-native"
  deno run -A scripts/bootstrap-phase9-kernel-smoke.mjs out/clapse_compiler.wasm
  @set +e; \
  CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler.wasm' deno run -A scripts/run-clapse-compiler-wasm.mjs compile examples/wasm_main.clapse out/wasm_main_from_phase9.wasm >/tmp/clapse_phase9_stdout.log 2>/tmp/clapse_phase9_stderr.log; \
  code=$?; \
  set -e; \
  test $code -ne 0; \
  grep -q "native compile not implemented yet" /tmp/clapse_phase9_stderr.log; \
  CLAPSE_ALLOW_HOST_COMPILE_FALLBACK=1 CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler.wasm' deno run -A scripts/run-clapse-compiler-wasm.mjs compile examples/wasm_main.clapse out/wasm_main_from_phase9_fallback.wasm >/tmp/clapse_phase9_fallback_stdout.log 2>/tmp/clapse_phase9_fallback_stderr.log; \
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse -- compile examples/wasm_main.clapse out/wasm_main_from_host_compare.wasm >/tmp/clapse_phase9_host_compare_stdout.log 2>/tmp/clapse_phase9_host_compare_stderr.log; \
  test "$(deno run -A scripts/run-wasm.mjs out/wasm_main_from_phase9_fallback.wasm main 7)" = "$(deno run -A scripts/run-wasm.mjs out/wasm_main_from_host_compare.wasm main 7)"; \
  test "$(printf 'id x = x\n' | CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler.wasm' deno run -A scripts/clapse.mjs format --stdin)" = "id x = x"; \
  echo "bootstrap phase9 compiler kernel smoke: PASS"

bootstrap-phase10-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase10_frontend_lexer.clapse out/bootstrap_phase10_frontend_lexer.wasm
  test "$(deno run -A scripts/run-wasm.mjs out/bootstrap_phase10_frontend_lexer.wasm main 0)" = "111001"

bootstrap-phase11-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase11_parser_combinator_pilot.clapse out/bootstrap_phase11_parser_combinator_pilot.wasm
  test "$(deno run -A scripts/run-wasm.mjs out/bootstrap_phase11_parser_combinator_pilot.wasm main 0)" = "-801815732"

bootstrap-check:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase1_frontend_primitives.clapse out/bootstrap_phase1.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase2_core_data_structures.clapse out/bootstrap_phase2.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase3_entry.clapse out/bootstrap_phase3.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase4_parser_pilot.clapse out/bootstrap_phase4_parser_pilot.wasm
  deno run -A scripts/bootstrap-parser-pilot-smoke.mjs out/bootstrap_phase4_parser_pilot.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase5_dispatch_pilot.clapse out/bootstrap_phase5_dispatch_pilot.wasm
  deno run -A scripts/bootstrap-phase5-dispatch-smoke.mjs out/bootstrap_phase5_dispatch_pilot.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase6_entry.clapse out/bootstrap_phase6_entry.wasm
  deno run -A scripts/bootstrap-phase6-module-smoke.mjs out/bootstrap_phase6_entry.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase7_host_capability_pilot.clapse out/bootstrap_phase7_host_capability_pilot.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase8_pattern_and_operators.clapse out/bootstrap_phase8_pattern_and_operators.wasm
  test "$(deno run -A scripts/run-wasm.mjs out/bootstrap_phase8_pattern_and_operators.wasm main 0)" = "0"
  test "$(deno run -A scripts/run-wasm.mjs out/bootstrap_phase8_pattern_and_operators.wasm main 7)" = "7"
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bootstrap_phase9_compiler_kernel.clapse out/clapse_compiler.wasm
  deno run -A scripts/bootstrap-phase9-kernel-smoke.mjs out/clapse_compiler.wasm
  just bootstrap-phase10-smoke
  just bootstrap-phase11-smoke
  test "$(CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler.wasm' deno run -A scripts/run-clapse-compiler-wasm.mjs engine-mode)" = "wasm-native"
  echo "bootstrap-check: PASS"

parity-check:
  mkdir -p .cabal-logs
  deno run -A scripts/check-selfhost-manifests.mjs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal test
  just bootstrap-check
  deno run -A scripts/selfhost-parser-parity.mjs --manifest examples/selfhost_parser_corpus.txt --out out/selfhost-parser-parity
  deno run -A scripts/formatter-idempotence-corpus.mjs --manifest examples/compiler_source_corpus.txt --out out/formatter-idempotence
  just selfhost-build-wasm-bridge
  CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler_bridge.wasm' deno run -A scripts/lsp-wasm-fixtures.mjs
  deno run -A scripts/selfhost-diff.mjs --manifest examples/selfhost_corpus.txt --out out/selfhost-diff
  deno run -A scripts/selfhost-behavior-diff.mjs --manifest examples/selfhost_behavior_corpus.json --out out/selfhost-behavior-diff
  echo "parity-check: PASS"

selfhost-parser-parity:
  mkdir -p .cabal-logs
  deno run -A scripts/selfhost-parser-parity.mjs --manifest examples/selfhost_parser_corpus.txt --out out/selfhost-parser-parity

selfhost-parser-parity-strict:
  mkdir -p .cabal-logs
  left="${SELFHOST_LEFT_CMD:-CABAL_DIR=\"$PWD/.cabal\" CABAL_LOGDIR=\"$PWD/.cabal-logs\" cabal run clapse --}"; \
  right="${SELFHOST_RIGHT_CMD:-CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH=\"${CLAPSE_COMPILER_WASM_PATH:-out/clapse_compiler_bridge.wasm}\" deno run -A scripts/run-clapse-compiler-wasm.mjs --}"; \
  deno run -A scripts/selfhost-parser-parity.mjs --manifest examples/selfhost_parser_corpus.txt --left-name haskell --right-name wasm --left "$left" --right "$right" --require-distinct-engines 1 --require-right-engine-mode wasm --out out/selfhost-parser-parity

formatter-idempotence-corpus:
  mkdir -p .cabal-logs
  deno run -A scripts/formatter-idempotence-corpus.mjs --manifest examples/compiler_source_corpus.txt --out out/formatter-idempotence

lsp-wasm-fixtures:
  : "${CLAPSE_COMPILER_WASM_PATH:?set CLAPSE_COMPILER_WASM_PATH to compiler wasm artifact}"
  deno run -A scripts/lsp-wasm-fixtures.mjs

selfhost-artifacts entry='examples/bootstrap_phase6_entry.clapse' out='out/selfhost-artifacts':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- selfhost-artifacts {{entry}} {{out}}

selfhost-diff:
  mkdir -p .cabal-logs
  deno run -A scripts/selfhost-diff.mjs --manifest examples/selfhost_corpus.txt --out out/selfhost-diff

selfhost-diff-strict:
  mkdir -p .cabal-logs
  left="${SELFHOST_LEFT_CMD:-CABAL_DIR=\"$PWD/.cabal\" CABAL_LOGDIR=\"$PWD/.cabal-logs\" cabal run clapse --}"; \
  right="${SELFHOST_RIGHT_CMD:-CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH=\"${CLAPSE_COMPILER_WASM_PATH:-out/clapse_compiler_bridge.wasm}\" deno run -A scripts/run-clapse-compiler-wasm.mjs --}"; \
  deno run -A scripts/selfhost-diff.mjs --manifest examples/selfhost_corpus.txt --left-name haskell --right-name wasm --left "$left" --right "$right" --require-distinct-engines 1 --require-right-engine-mode wasm --out out/selfhost-diff

selfhost-behavior-diff:
  mkdir -p .cabal-logs
  deno run -A scripts/selfhost-behavior-diff.mjs --manifest examples/selfhost_behavior_corpus.json --out out/selfhost-behavior-diff

selfhost-behavior-diff-strict:
  mkdir -p .cabal-logs
  left="${SELFHOST_LEFT_CMD:-CABAL_DIR=\"$PWD/.cabal\" CABAL_LOGDIR=\"$PWD/.cabal-logs\" cabal run clapse --}"; \
  right="${SELFHOST_RIGHT_CMD:-CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH=\"${CLAPSE_COMPILER_WASM_PATH:-out/clapse_compiler_bridge.wasm}\" deno run -A scripts/run-clapse-compiler-wasm.mjs --}"; \
  deno run -A scripts/selfhost-behavior-diff.mjs --manifest examples/selfhost_behavior_corpus.json --left-name haskell --right-name wasm --left "$left" --right "$right" --require-distinct-engines 1 --require-right-engine-mode wasm --out out/selfhost-behavior-diff

selfhost-bootstrap-abc:
  mkdir -p .cabal-logs
  deno run -A scripts/selfhost-bootstrap-abc.mjs --manifest examples/selfhost_corpus.txt --behavior-manifest examples/selfhost_behavior_corpus.json --out out/selfhost-bootstrap

selfhost-bootstrap-abc-strict:
  mkdir -p .cabal-logs
  left="${SELFHOST_LEFT_CMD:-CABAL_DIR=\"$PWD/.cabal\" CABAL_LOGDIR=\"$PWD/.cabal-logs\" cabal run clapse --}"; \
  right="${SELFHOST_RIGHT_CMD:-CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH=\"${CLAPSE_COMPILER_WASM_PATH:-out/clapse_compiler_bridge.wasm}\" deno run -A scripts/run-clapse-compiler-wasm.mjs --}"; \
  deno run -A scripts/selfhost-bootstrap-abc.mjs --manifest examples/selfhost_corpus.txt --behavior-manifest examples/selfhost_behavior_corpus.json --left-name haskell --right-name wasm --left "$left" --right "$right" --require-distinct-engines 1 --require-right-engine-mode wasm --out out/selfhost-bootstrap

selfhost-bench repeats='1':
  mkdir -p .cabal-logs
  deno run -A scripts/selfhost-bench.mjs --manifest examples/selfhost_behavior_corpus.json --repeats {{repeats}} --out out/selfhost-bench

selfhost-bench-wasm repeats='1':
  mkdir -p .cabal-logs
  : "${CLAPSE_COMPILER_WASM_PATH:?set CLAPSE_COMPILER_WASM_PATH to compiler wasm artifact}"
  left='CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --'; \
  right='CLAPSE_COMPILER_WASM_PATH="$CLAPSE_COMPILER_WASM_PATH" deno run -A scripts/run-clapse-compiler-wasm.mjs --'; \
  deno run -A scripts/selfhost-bench.mjs --manifest examples/selfhost_behavior_corpus.json --repeats {{repeats}} --left-name haskell --right-name wasm --left "$left" --right "$right" --require-distinct-engines 1 --require-right-engine-mode wasm --out out/selfhost-bench-wasm

selfhost-bench-wasm-fresh repeats='1':
  mkdir -p .cabal-logs
  : "${CLAPSE_COMPILER_WASM_PATH:?set CLAPSE_COMPILER_WASM_PATH to compiler wasm artifact}"
  left='CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --'; \
  right='CLAPSE_COMPILER_WASM_PATH="$CLAPSE_COMPILER_WASM_PATH" deno run -A scripts/run-clapse-compiler-wasm.mjs --'; \
  deno run -A scripts/selfhost-bench.mjs --manifest examples/selfhost_behavior_corpus.json --repeats {{repeats}} --left-name haskell --right-name wasm --left "$left" --right "$right" --require-distinct-engines 1 --require-right-engine-mode wasm --reuse-compiles-across-repeats 0 --out out/selfhost-bench-wasm-fresh

selfhost-check:
  mkdir -p .cabal-logs
  deno run -A scripts/check-selfhost-manifests.mjs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal test
  just bootstrap-check
  just selfhost-parser-parity
  just formatter-idempotence-corpus
  just selfhost-build-wasm-bridge
  CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler_bridge.wasm' just lsp-wasm-fixtures
  just selfhost-diff
  just selfhost-behavior-diff
  just selfhost-bootstrap-abc
  echo "selfhost-check: PASS"

selfhost-check-strict:
  mkdir -p .cabal-logs
  deno run -A scripts/check-selfhost-manifests.mjs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal test
  just bootstrap-check
  just selfhost-build-wasm-bridge
  just selfhost-parser-parity-strict
  just formatter-idempotence-corpus
  CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler_bridge.wasm' just lsp-wasm-fixtures
  just selfhost-diff-strict
  just selfhost-behavior-diff-strict
  just selfhost-bootstrap-abc-strict
  echo "selfhost-check-strict: PASS"

selfhost-check-wasm:
  mkdir -p .cabal-logs
  : "${CLAPSE_COMPILER_WASM_PATH:?set CLAPSE_COMPILER_WASM_PATH to compiler wasm artifact}"
  SELFHOST_RIGHT_CMD='CLAPSE_COMPILER_WASM_PATH="$CLAPSE_COMPILER_WASM_PATH" deno run -A scripts/run-clapse-compiler-wasm.mjs --' just selfhost-check-strict

selfhost-build-wasm-bridge out='out/clapse_compiler_bridge.wasm':
  deno run -A scripts/build-compiler-wasm-bridge.mjs {{out}}

selfhost-check-wasm-bridge:
  mkdir -p .cabal-logs
  just selfhost-build-wasm-bridge
  CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH='out/clapse_compiler_bridge.wasm' just selfhost-check-wasm

wasm-linear-memory-helpers-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_linear_memory_helpers.clapse out/wasm_linear_memory_helpers.wasm
  deno run -A scripts/wasm-linear-memory-helpers-smoke.mjs out/wasm_linear_memory_helpers.wasm

bench iters='200000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- bench {{iters}}

bench-wasm wasm='out/wasm_main.wasm' entry='main' iters='2000000' warmup='20000':
  deno run -A scripts/bench-wasm.mjs {{wasm}} {{entry}} {{iters}} {{warmup}}

bench-wasm-main iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_main.clapse out/wasm_main.wasm
  deno run -A scripts/bench-wasm.mjs out/wasm_main.wasm main {{iters}} {{warmup}}

bench-wasm-compare iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_hand.clapse out/bench_wasm_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_abstraction.clapse out/bench_wasm_abstraction.wasm
  echo "=== hand ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-closure-env iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_closure_env_hand.clapse out/bench_wasm_closure_env_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_closure_env_abstraction.clapse out/bench_wasm_closure_env_abstraction.wasm
  echo "=== hand ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_closure_env_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_closure_env_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-struct-field iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_struct_field_hand.clapse out/bench_wasm_struct_field_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_struct_field_abstraction.clapse out/bench_wasm_struct_field_abstraction.wasm
  echo "=== hand ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_struct_field_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_struct_field_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-wrapper-uncurry iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_wrapper_uncurry_hand.clapse out/bench_wasm_wrapper_uncurry_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_wrapper_uncurry_abstraction.clapse out/bench_wasm_wrapper_uncurry_abstraction.wasm
  echo "=== hand ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-slice-set iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_slice_set_reuse.clapse out/bench_wasm_slice_set_reuse.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_slice_set_copy.clapse out/bench_wasm_slice_set_copy.wasm
  echo "=== reuse (no memcpy expected) ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_slice_set_reuse.wasm main {{iters}} {{warmup}}
  echo "=== copy (memcpy path expected) ==="
  deno run -A scripts/bench-wasm.mjs out/bench_wasm_slice_set_copy.wasm main {{iters}} {{warmup}}

bench-wasm-compare-all iters='2000000' warmup='20000':
  just bench-wasm-compare {{iters}} {{warmup}}
  just bench-wasm-compare-closure-env {{iters}} {{warmup}}
  just bench-wasm-compare-struct-field {{iters}} {{warmup}}
  just bench-wasm-compare-wrapper-uncurry {{iters}} {{warmup}}
  just bench-wasm-compare-slice-set {{iters}} {{warmup}}

js arg='7':
  deno run -A scripts/run-wasm.mjs out/wasm_main.wasm main {{arg}}

life-build:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/game_of_life.clapse out/game_of_life.wasm

life-smoke:
  just life-build
  test "$(deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 0 3)" = "1"
  test "$(deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 1 2)" = "1"
  test "$(deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 1 4)" = "0"
  deno run -A scripts/life-slice-smoke.mjs out/game_of_life.wasm
  echo "life smoke: PASS"

life-time width='160' height='100' iterations='200' jump='1':
  just life-build
  deno run -A time-life-step.mjs {{width}} {{height}} {{iterations}} {{jump}}

life-serve port='8080':
  just life-build
  echo "open http://localhost:{{port}}/examples/game_of_life.html"
  python3 -m http.server {{port}}

mario-build:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/mario_ecs.clapse out/mario_ecs.wasm

mario-smoke:
  just mario-build
  deno run -A scripts/mario-ecs-smoke.mjs out/mario_ecs.wasm

mario-serve port='8080':
  just mario-build
  echo "open http://localhost:{{port}}/examples/mario_ecs.html"
  python3 -m http.server {{port}}
