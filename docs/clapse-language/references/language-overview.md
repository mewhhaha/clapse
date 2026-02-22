# Clapse Language Overview

## Purpose

Clapse is a pure functional toy language that compiles to WASM and prioritizes aggressive compile-time collapse/reduction.

## Core Design Constraints

- Keep core semantics pure to maximize deterministic rewrites.
- Treat side effects as host boundary concerns (for example JS driving wasm).
- Optimize abstractions toward hand-written equivalent code where safe.
- Keep a verifier in the pipeline so optimization remains constrained and auditable.

## Current Implemented Surface

- Function definitions and application
- Lambda/closure support
- `let ... in ...` local bindings
- `case ... of` with multiple scrutinees and constructor patterns
- `data` declarations with multiple constructors and one-line GADT style
- Type signatures with optional named witnesses in constraints
- Class/law/instance declarations for compile-time rewrites
- Builtin infix operators (`+ - * / == &&`) plus custom infix operators with fixity and precedence
- Collection literals (`[]`, `[a, b, ...]`) lowered via collection builtins
- Lowering to stack ops, then collapsed IR normalization and verification
- WASM emission with closure/data/string runtime interop
- Formatter + LSP in same executable

## Non-Goals / Current Gaps

- No `do`/`ado` notation
- No full proof checker yet
- No complete numeric-width backend ABI (`u64`/`byte` runtime paths incomplete)
- Richer linear-memory value layout for structs/strings and slice descriptors

## Purity and Host Boundary

- Clapse source stays pure.
- Mutations are expected at host boundaries (for example JS event loop, canvas, IO).
- WASM runtime values are represented through tagged integers plus linear-memory records/descriptors.

## Decision Rule for New Features

- Add a feature only if it preserves pure-core reasoning and can be reduced/collapsed predictably.
- Keep proposal text separate from current implementation status.
