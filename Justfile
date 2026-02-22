set shell := ["bash", "-euo", "pipefail", "-c"]

default:
  @just --list

# Full local DX bootstrap: compiler, formatter/LSP smoke checks, tree-sitter, and Helix wiring.
install:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal build --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary exe:clapse
  printf 'id x = x\n' | CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- format --stdin >/dev/null
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- lsp --stdio </dev/null >/dev/null
  ./scripts/setup-helix-local.sh
  hx --health clapse

grammar:
  cd tree-sitter-clapse && tree-sitter generate
  cd tree-sitter-clapse && XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp}" tree-sitter test

helix:
  ./scripts/setup-helix-local.sh

wasm-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_main.clapse out/wasm_main.wasm
  node scripts/run-wasm.mjs out/wasm_main.wasm main 7

wasm-closure-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_closure.clapse out/wasm_closure.wasm
  node scripts/run-wasm.mjs out/wasm_closure.wasm main 7

wasm-string-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/strings.clapse out/wasm_strings.wasm
  node scripts/run-wasm.mjs out/wasm_strings.wasm main

wasm-interop-slice-build:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/interop_slice.clapse out/interop_slice.wasm

wasm-interop-slice-smoke:
  just wasm-interop-slice-build
  output="$(node examples/interop_slice.mjs out/interop_slice.wasm)"; \
  echo "$output"; \
  test "$(printf '%s\n' "$output" | sed -n '1p')" = "GET 1"; \
  test "$(printf '%s\n' "$output" | sed -n '2p')" = "POST 0"

wasm-runtime-contract-smoke:
  node scripts/runtime-contract-smoke.mjs

wasm-struct-helpers-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_has_tag.clapse out/wasm_struct_has_tag.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_has_tag_false.clapse out/wasm_struct_has_tag_false.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_get_ok.clapse out/wasm_struct_get_ok.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_struct_get_mismatch.clapse out/wasm_struct_get_mismatch.wasm
  node scripts/wasm-struct-helpers-smoke.mjs out/wasm_struct_has_tag.wasm out/wasm_struct_has_tag_false.wasm out/wasm_struct_get_ok.wasm out/wasm_struct_get_mismatch.wasm

wasm-linear-memory-helpers-smoke:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_linear_memory_helpers.clapse out/wasm_linear_memory_helpers.wasm
  node scripts/wasm-linear-memory-helpers-smoke.mjs out/wasm_linear_memory_helpers.wasm

bench iters='200000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- bench {{iters}}

bench-wasm wasm='out/wasm_main.wasm' entry='main' iters='2000000' warmup='20000':
  node scripts/bench-wasm.mjs {{wasm}} {{entry}} {{iters}} {{warmup}}

bench-wasm-main iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/wasm_main.clapse out/wasm_main.wasm
  node scripts/bench-wasm.mjs out/wasm_main.wasm main {{iters}} {{warmup}}

bench-wasm-compare iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_hand.clapse out/bench_wasm_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_abstraction.clapse out/bench_wasm_abstraction.wasm
  echo "=== hand ==="
  node scripts/bench-wasm.mjs out/bench_wasm_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  node scripts/bench-wasm.mjs out/bench_wasm_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-closure-env iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_closure_env_hand.clapse out/bench_wasm_closure_env_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_closure_env_abstraction.clapse out/bench_wasm_closure_env_abstraction.wasm
  echo "=== hand ==="
  node scripts/bench-wasm.mjs out/bench_wasm_closure_env_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  node scripts/bench-wasm.mjs out/bench_wasm_closure_env_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-struct-field iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_struct_field_hand.clapse out/bench_wasm_struct_field_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_struct_field_abstraction.clapse out/bench_wasm_struct_field_abstraction.wasm
  echo "=== hand ==="
  node scripts/bench-wasm.mjs out/bench_wasm_struct_field_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  node scripts/bench-wasm.mjs out/bench_wasm_struct_field_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-wrapper-uncurry iters='2000000' warmup='20000':
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_wrapper_uncurry_hand.clapse out/bench_wasm_wrapper_uncurry_hand.wasm
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/bench_wasm_wrapper_uncurry_abstraction.clapse out/bench_wasm_wrapper_uncurry_abstraction.wasm
  echo "=== hand ==="
  node scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_hand.wasm main {{iters}} {{warmup}}
  echo "=== abstraction ==="
  node scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_abstraction.wasm main {{iters}} {{warmup}}

bench-wasm-compare-all iters='2000000' warmup='20000':
  just bench-wasm-compare {{iters}} {{warmup}}
  just bench-wasm-compare-closure-env {{iters}} {{warmup}}
  just bench-wasm-compare-struct-field {{iters}} {{warmup}}
  just bench-wasm-compare-wrapper-uncurry {{iters}} {{warmup}}

js arg='7':
  node scripts/run-wasm.mjs out/wasm_main.wasm main {{arg}}

life-build:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/game_of_life.clapse out/game_of_life.wasm

life-smoke:
  just life-build
  test "$(node scripts/run-wasm.mjs out/game_of_life.wasm main 0 3)" = "1"
  test "$(node scripts/run-wasm.mjs out/game_of_life.wasm main 1 2)" = "1"
  test "$(node scripts/run-wasm.mjs out/game_of_life.wasm main 1 4)" = "0"
  node scripts/life-slice-smoke.mjs out/game_of_life.wasm
  echo "life smoke: PASS"

life-time width='160' height='100' iterations='200' jump='1':
  just life-build
  node time-life-step.mjs {{width}} {{height}} {{iterations}} {{jump}}

life-serve port='8080':
  just life-build
  echo "open http://localhost:{{port}}/examples/game_of_life.html"
  python3 -m http.server {{port}}

mario-build:
  mkdir -p .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --build-log=./.cabal-logs/build.log --build-summary=./.cabal-logs/build.summary -- compile examples/mario_ecs.clapse out/mario_ecs.wasm

mario-smoke:
  just mario-build
  node scripts/mario-ecs-smoke.mjs out/mario_ecs.wasm

mario-serve port='8080':
  just mario-build
  echo "open http://localhost:{{port}}/examples/mario_ecs.html"
  python3 -m http.server {{port}}
