# Parallel Runtime Principles by Field

## Use When

Use this when you want parallel runtime guidance that is not tied to one project, but to core systems/compiler fields.

## 1. PL Semantics and Rewriting

Goal: allow safe reordering of work without changing meaning.

Principles:

- favor semantics with explicit commutation/independence regions
- require local rewrite rules with machine-checkable preconditions
- keep effectful operations explicit so optimizer/scheduler can respect order
- separate semantic equivalence rewrites from cost-driven rewrites

Checklist:

1. define when two reductions/tasks are independent
2. prove or test confluence/commutation where claimed
3. attach proof/evidence to aggressive rewrites

## 2. Runtime Scheduling and Work Distribution

Goal: high utilization with low contention and predictable behavior.

Mode note:

- throughput-first mode: local queues + bounded stealing is a strong default
- determinism-first mode: use deterministic queues/actors; treat stealing as opt-in with strict constraints

Principles:

- use local work queues first, then stealing
- steal coarse-enough tasks to avoid overhead domination
- keep scheduling metadata compact and cheap to move
- cap retries/spins and provide starvation backoff

Checklist:

1. per-worker local queue + bounded stealing policy
2. deterministic tie-breakers in debug/replay modes
3. throughput and tail-latency metrics per scheduler mode

## 3. Lock-Free Shared-State Patterns

Goal: minimize lock contention in hot coordination paths.

Principles:

- use flat indexed structures when identity is numeric/stable
- prefer single-word atomic operations for common transitions
- encode ownership/state transitions explicitly (`empty`, `pending`, `done`)
- verify memory-order assumptions with stress and model tests

Checklist:

1. document every atomic state transition
2. run race/stress tests under high contention
3. include fallback path when lock-free path is saturated

## 4. GPU Workload Shaping and Divergence Control

Goal: keep warps/lanes doing similar work to avoid serialized execution.

Principles:

- batch or tag tasks by shape/cost class before sharing
- avoid assigning structurally different work to neighboring lanes
- use block/local coordination more than global synchronization
- tune share frequency and chunk size empirically

Checklist:

1. classify task shapes for scheduling decisions
2. measure divergence, occupancy, and memory stalls separately
3. keep scalar/CPU fallback for non-GPU-friendly workloads

## 5. Memory Hierarchy and Locality

Goal: keep hot-path allocations and accesses close to compute units.

Principles:

- use local/scratch arenas for short-lived intermediates
- define explicit promotion path from local to shared/global visibility
- avoid random pointer chasing in hot loops
- separate hot metadata from cold payloads

Checklist:

1. profile cache/shared/global memory hit patterns
2. validate promotion correctness under parallel races
3. keep addressing-width assumptions behind feature gates

## 6. Compiler Hints and Parallel Annotations

Goal: expose parallel structure to runtime without hard-coding policy.

Principles:

- let compiler mark likely parallelizable branches/tasks
- treat hints as advisory, not semantic requirements
- keep hints stable and versioned in IR/metadata
- allow runtime to ignore hints when they hurt performance

Checklist:

1. add hint quality metrics (helpful vs harmful)
2. keep hint generation deterministic across builds
3. test behavior with hints enabled and disabled

## 7. Predictability, Safety, and Operations

Goal: avoid “fast but opaque” runtime behavior.

Principles:

- keep baseline sequential semantics as reference oracle
- require guard + rollback/deopt for speculative runtime rewrites
- log scheduling and optimization decisions in replayable traces
- separate safe-default and experimental runtime modes

Checklist:

1. differential tests against baseline mode
2. deterministic replay for production failures
3. rollback switch for each aggressive optimization family

## Pointers

- Deterministic policy: `references/recipes/deterministic-parallel-evaluation.md`
- Parallel primitives: `references/recipes/parallelism-threads.md`
- Runtime rewrite safety: `references/recipes/proof-guided-runtime-wasm-rewrites.md`
