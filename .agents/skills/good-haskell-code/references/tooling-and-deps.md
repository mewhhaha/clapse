# Tooling and Dependencies (GHC 9.14.1)

Prefer `default-language: GHC2024` at the component level, then add only module-level extension deltas that are truly needed.

## Contents
- 1. Package matrix by concern
- 2. Cabal component layout
- 3. Build/profile flags strategy
- 4. GHCi and ghcid loops
- 5. foreign-store dependency rules
- 6. Template Haskell dependency rules
- 7. Good/Bad dependency hygiene

## 1) Package matrix by concern

Use only what the component needs.

```txt
Core runtime:
- base, containers, text, bytestring

Concurrency:
- stm, async

Performance data paths:
- vector, deepseq

Persistence/wire:
- binary and/or aeson

Hot reload state (dev-focused):
- foreign-store

Benchmarking:
- criterion (benchmark stanza only)

Compile-time generation/caching:
- template-haskell
- file-embed (optional)
```

Avoid pulling benchmarking/testing libs into production libraries.

## 2) Cabal component layout

```cabal
library
  hs-source-dirs: src
  default-language: GHC2024
  build-depends:
      base
    , containers
    , text
    , bytestring
    , stm
    , async
    , vector
    , deepseq
    , binary

executable game-main
  hs-source-dirs: app
  main-is: Main.hs
  default-language: GHC2024
  build-depends:
      base
    , my-lib
  ghc-options: -O2 -threaded -rtsopts

benchmark perf
  type: exitcode-stdio-1.0
  hs-source-dirs: bench
  main-is: Main.hs
  default-language: GHC2024
  build-depends:
      base
    , my-lib
    , criterion
  ghc-options: -O2 -threaded -rtsopts
```

Why: component-scoped dependencies keep compile times and API surface under control.

## 3) Build/profile flags strategy

```txt
Development default:
-O1 or -O2, -Wall, -Wcompat, -rtsopts

Profile build:
--enable-profiling + -fprof-auto (only when profiling)

Eventlog build:
-eventlog -threaded -rtsopts
```

For linear ownership APIs in hot modules:

```txt
Use module-level extension:
{-# LANGUAGE LinearTypes #-}
```

Enable `LinearTypes` only where ownership constraints are intentional and reviewed.

```txt
GOOD:
Separate normal, profiling, and eventlog builds.

BAD:
Always-on profiling flags in default dev builds.
```

Why: profiled/eventlog binaries behave differently; do not use them for final throughput claims.

## 4) GHCi and ghcid loops

```bash
# Library reload loop
ghcid --command="cabal repl lib:my-lib"

# App loop with quick checks
ghcid --command="cabal repl exe:game-main" --test=":main --dry-run"
```

```txt
GOOD:
Use ghcid for rapid compile feedback, then run full tests/bench before conclusions.

BAD:
Use ghcid success as the only release gate.
```

For hot reload with runtime state:
- keep runtime state in `TVar`/`MVar`;
- swap behavior functions after reload;
- keep fallback behavior if reload fails.

## 5) foreign-store dependency rules

`foreign-store` is for in-process state persistence across `ghci :reload` in development.
It is not for cross-process shared memory.

```cabal
library
  if flag(dev-hot-reload)
    build-depends: foreign-store
```

```txt
GOOD:
Gate foreign-store usage behind a dev flag and wrap it behind a small module API.

BAD:
Make foreign-store a hard dependency for all production components.
```

Why: keep hot-reload ergonomics without forcing runtime architecture around a dev-only tool.

## 6) Template Haskell dependency rules

Use Template Haskell in dedicated generator modules.

```cabal
library
  build-depends:
      base
    , template-haskell
    , file-embed
```

```txt
GOOD:
- keep TH + file-embed optional and localized to modules that need compile-time generation.
- keep normal runtime modules TH-free where practical.

BAD:
- sprinkle TH splices through core business modules.
- make huge generated expressions part of every module compile.
```

Why: isolating TH keeps compile behavior predictable and simplifies debugging.

## 7) Good/Bad dependency hygiene

```txt
GOOD:
- keep version bounds explicit and reasonable;
- keep benchmark/profiling deps out of library APIs;
- prefer module-local imports over prelude-like umbrellas;
- review dependency additions for startup/alloc impact.
```

```txt
BAD:
- add async/vector/aeson everywhere \"just in case\";
- tie production runtime to dev-only hot-reload packages;
- mix profiling and release flags in one default build path.
```
