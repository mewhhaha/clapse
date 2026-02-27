# Clapse Optimization Research Ledger (Living)

Status: ongoing  
Scope: mathematical, categorical, and functional-theory invariants that justify optimization passes.

This document is the gate for optimization design.  
If a proposed optimization cannot be justified by these invariants (or an equivalent formally stated invariant), it should not land.

## 1. Optimization policy (strict, non-ad-hoc)

Every optimization must satisfy all of:

1. Semantics-preserving by construction:
   - Equational law, type theorem, or verified refinement relation.
2. Explicit preconditions:
   - Typed guard predicates, purity/effect guards, and shape constraints.
3. Deterministic application:
   - No heuristic rewrites that can silently change semantics.
4. Bounded convergence:
   - Fixed-point or saturation passes must have explicit termination/resource bounds.
5. Verifiability:
   - Translation validation, equivalence checks, or proof artifacts for critical transforms.

## 2. Core invariants we optimize under

## 2.1 Equational reasoning over pure terms

- Invariant: rewrites apply only where referential transparency holds.
- Consequence: rewrites are law-driven (identity, associativity variants, fusion, annihilation, idempotence) under typed guards.
  Constant-negation is in this same cluster (`not true -> false`, `not false -> true`) and requires bool+pure guard discipline.

## 2.2 Type-directed admissibility

- Invariant: rewrite preconditions include type compatibility checks.
- Consequence: rules are denied for mismatched or effectful terms even if syntactic shape matches.

## 2.3 Effect and strictness discipline

- Invariant: effect tracking bounds what can be erased/reordered.
- Consequence: strictness/demand-based rewrites only eliminate computations proven pure/unused under semantics.

## 2.4 Linearity and alias-aware memory model

- Invariant: single-use (linear) ownership enables in-place updates; shared/escaped ownership requires copy-on-write.
- Consequence: memory optimizations are represented as ownership-policy rewrites, not ad-hoc mutation shortcuts.

## 2.5 Congruence-closed rewrite spaces

- Invariant: equivalence classes (e-graphs or equivalent structures) represent many equal programs without phase-order bias.
- Consequence: extraction chooses profitable representatives; rewrite order does not encode correctness assumptions.

## 2.6 Verified refinement / translation validation

- Invariant: high-value optimizations should be checkable post-pass (bounded validation accepted where necessary).
- Consequence: correctness can be enforced independently of optimizer implementation details.

## 3. Category/function-theory framing used here

- Endofunctor laws: `map id = id`, `map f . map g = map (f . g)`.
- Monoid-like boolean algebra fragments (within typed boolean domain): identity, annihilation, idempotence, double negation.
- Compositionality: whole-program correctness built from pass-local semantic preservation.
- CBPV-style separation of values/computations as a clean place to attach effects/coeffects for optimizer safety.
- Graded/linear modalities as a static foundation for alias/resource-sensitive rewrites.

## 4. Pass admission checklist (must be answered in PR/commit)

1. Which law/theorem authorizes this rewrite?
2. What are the typed/effect/shape guards?
3. Why is rule ordering irrelevant for correctness?
4. What bounds ensure termination/convergence?
5. How is equivalence checked (proof, validator, differential behavior test)?
6. What counterexample class is intentionally rejected by guards?

## 5. Research evidence ledger (modern, source-linked, quoted)

Notes:
- Quotes are intentionally short and direct.
- Each quote is used as evidence for a specific invariant above.

1. Max Willsey et al., *egg: Fast and Extensible Equality Saturation* (POPL 2021 / arXiv 2020).  
   Link: https://arxiv.org/abs/2004.03082  
   Quote: "An e-graph efficiently represents a congruence relation over many expressions."  
   Why it matters: congruence-closed equivalence space for non-ad-hoc rewrite composition.

2. Thomas Koehler et al., *Sketch-Guided Equality Saturation* (2021).  
   Link: https://arxiv.org/abs/2111.13040  
   Quote: "encoding polymorphically typed lambda calculi ... reduces the runtime and memory consumption ... by orders of magnitude."  
   Why it matters: type-aware encodings are practical enablers, not just theory.

3. Jules Merckx et al., *E-Graphs as a Persistent Compiler Abstraction* (2026).  
   Link: https://arxiv.org/abs/2602.16707  
   Quote: "represents an e-graph natively in the compiler's intermediate representation."  
   Why it matters: persistent equality knowledge across pass boundaries.

4. Jiaqi Yin et al., *HEC: Equivalence Verification Checking for Code Transformation via Equality Saturation* (2025).  
   Link: https://arxiv.org/abs/2506.02290  
   Quote: "identified two critical compilation errors in mlir-opt."  
   Why it matters: translation/equivalence checking catches real optimizer bugs.

5. Jean-Philippe Bernardy et al., *Linear Haskell* (2017).  
   Link: https://arxiv.org/abs/1710.09756  
   Quote: "attach linearity to function arrows."  
   Why it matters: linearity as a first-class typing tool for safe reuse/mutation-style optimization.

6. Jack Hughes et al., *Deriving Distributive Laws for Graded Linear Types* (2021).  
   Link: https://arxiv.org/abs/2112.14966  
   Quote: "automatically derive these distributive laws as combinators."  
   Why it matters: law derivation can be mechanized and reused by optimizers.

7. Cassia Torczon et al., *Effects and Coeffects in Call-By-Push-Value* (2023).  
   Link: https://arxiv.org/abs/2311.11795  
   Quote: "effect-and-coeffect soundness."  
   Why it matters: type-level guarantees for when elimination/reordering is legal.

8. Hector Suzanne and Emmanuel Chailloux, *A Reusable Machine-Calculus for Automated Resource Analyses* (2023).  
   Link: https://arxiv.org/abs/2310.14719  
   Quote: "a polymorphic and linear type system enhanced with a first-order logical fragment."  
   Why it matters: resource/alias reasoning can be encoded in typing and constraints.

9. Yijia Chen and Lionel Parreaux, *The Long Way to Deforestation* (2024).  
   Link: https://arxiv.org/abs/2410.02232  
   Quote: "Deforestation is a compiler optimization that removes intermediate data structure allocations."  
   Why it matters: allocation-removal as a principled, typed transformation target.

10. Hrutvik Kanabar et al., *PureCake: A Verified Compiler for a Lazy Functional Language* (PLDI 2023).  
    Link: https://doi.org/10.1145/3591259  
    (open abstract mirror: https://kar.kent.ac.uk/101697)  
    Quote: "the first such result for any lazy language."  
    Why it matters: end-to-end verified optimization in a lazy functional setting is achievable.

11. Hrutvik Kanabar et al., *Verified Inlining and Specialisation for PureCake* (ESOP 2024).  
    Link: https://doi.org/10.1007/978-3-031-57267-8_11  
    Quote: "first verified inliner for a lazy functional programming language."  
    Why it matters: non-trivial optimizer passes can be proven without abandoning performance goals.

12. Nuno P. Lopes et al., *Alive2: bounded translation validation for LLVM* (PLDI 2021).  
    Link: https://doi.org/10.1145/3453483.3454030  
    (summary page: https://dblp.org/rec/conf/pldi/LopesLHLR21.html)  
    Quote: "bounded translation validation."  
    Why it matters: practical correctness checking under realistic bounds.

13. CompCert project documentation (current verified compiler pipeline).  
    Link: https://compcert.org/doc/  
    Quote: "semantic preservation theorems."  
    Why it matters: optimizer correctness must compose into whole-compiler guarantees.

14. David Monniaux and Cyril Six, *Simple, Light, Yet Formally Verified, Global CSE and LICM* (2021).  
    Link: https://arxiv.org/abs/2105.01344  
    Quote: "formally certified loop-invariant code motion optimization."  
    Why it matters: classical optimizations can be both lightweight and formally verified.

15. Ling Zhang et al., *Fully Composable and Adequate Verified Compilation with Direct Refinements between Open Modules* (2023).  
    Link: https://arxiv.org/abs/2302.12990  
    Quote: "directly relate native semantics of open modules."  
    Why it matters: compositional refinement is the right unit for modular optimization correctness.

16. Beniamino Accattoli and Adrienne Lancelot, *Mirroring Call-by-Need, or Values Acting Silly* (2024).  
    Link: https://arxiv.org/abs/2402.12078  
    Quote: "blindness with respect to efficiency of call-by-value contextual equivalence."  
    Why it matters: contextual equivalence alone is not enough; optimization needs explicit cost/resource invariants.

## 6. Practical implications for Clapse optimizer evolution

- Keep rewrite rules in explicit registries with typed/effect guards.
- Keep boolean rewrites in registry-consensus fixed-point groups with static dispatch guards and explicit convergence bounds.
- Prefer law schemas over one-off peephole transforms.
- Treat factoring rewrites as `ClassLawRule` registry rewrites with boolean-domain and purity guards; enforce one-way size-reducing orientation (`x && (x && y) -> x && y`, `x || (x || y) -> x || y`) to preserve invariants and convergence.
- Treat compose-associativity similarly as one-way canonicalization in registry with explicit compose-shape/type/purity guards (`compose f (compose g h) -> compose (compose f g) h`); do not add the reverse rewrite to prevent oscillation.
- Duplicate ad-hoc bool-collapse helper paths were removed in favor of a single, shared class-law registry (`ClassLawRule`) for boolean simplification.
- Current boolean simplification remains a consensus fixed-point rewrite cluster (`ClassLawRule`) under existing type/effect/shape guard discipline.
- Constant-negation laws are also admitted through the same cluster under bool+pure guards and strict cost-decrease policy in fixed-point governance.
- Associative-idempotence chain reductions are now admitted only through the class-law registry cluster with static-dispatch-only selection, bool-type guards, and pure-effect admissibility; orientation is size-reducing (or cost-neutral in map-fusion cases) under fixed-point iteration.
- Compose and map registry rewrites use bounded cost governance (`0` default growth, `+1` map-fusion exception); boolean-class-law rewrites additionally require strict cost decrease on each rewrite step.
- Strictness-mode is now derived from the class-law dispatch-state/signature-family bucketing, so strict decrease remains bool-only and compose/map remain budget-only while semantics stay unchanged.
- Root-shape rewrite scheduling now re-dispatches immediately after each successful rewrite, reclassifying the current expression before selecting the next rule bucket (`CCompose`, `CMap`, boolean forms).
  The redispatch key is `dispatch-state-key = (root-kind, signature-family)` for signature-aware in-pass scheduling, improving bucket selection correctness/perf.
  Dispatcher selection is now a precomputed deterministic root-kind + signature-family subset table lookup (constant-time) instead of a rule-list equality scan; guard predicates, `ClassDispatch*` gates, and strict-decrease/cost policy are unchanged.
- Expression signatures used by class-law guards are now cached once per expression state during each rewrite pass, and dispatch uses root-kind + signature-family gating to skip incompatible families before per-rule guard checks. This is an internal optimization; guard predicates, cost policy, dispatch mode, rewrite semantics, and policies are unchanged.
- Add pass metadata (status + invariant owner + proof/validator hook).
- Keep fixed-point passes bounded and cost-policy controlled.
- Couple optimization rollout with differential behavior checks and validator evidence.
- In this implementation, strict class-law boolean idempotence (`x && x`, `x || x`) is now active and constrained by boolean-type plus purity/effect guards.
- Boolean absorption and complement families are now admitted to the same law-first optimizer path (`ClassLawRule` fixed-point driver), with side-condition checks for boolean type + purity/strictness/effect metadata (`x && (x || y)`, `x || (x && y)`, `x && not x`, `x || not x`).
- Nested complement-chain annihilation rewrites are now included in the same registry cluster (`x && (not x || y)`, `x || (not x && y)` with outer-operand swaps), gated by `ClassDispatchStatic` and bool-only/pure-effect predicates, and enforced as one-way size-reducing rules in fixed-point iteration.

## 7. Open research-backed tasks

1. Add explicit law schema metadata to rewrite rules (`law_id`, `side_conditions`, `effect_requirements`, `type_requirements`).
2. Add optional equality-saturation mode for selected IR fragments (bounded e-graph budget).
3. Add translation-validation hooks for optimization-sensitive commands.
4. Expand alias/linearity typing to expose stronger in-place rewrite opportunities.
5. Encode demand/strictness as first-class analysis artifacts tied to rewrite admissibility.

## 8. Update protocol for this file

When adding a new optimization family:

1. Add or update the invariant section first.
2. Add at least one primary research citation with a short direct quote.
3. State the concrete Clapse pass-level implication.
4. Record what check/proof/validator will enforce it.

- Root-shape class-law selection now dispatches deterministically by expression root bucket before fixed-point rule matching through root-kind tag checks (`CCompose`/`CMap`/boolean forms); this is an optimization of selection order only and does not change guard predicates, `ClassDispatch*` gating, or strict-decrease/cost policy.
- `Other` is now an empty dispatch bucket because all active laws are root-specific (`CCompose`, `CMap`, boolean roots), so this is a scheduler-internal change and rewrite semantics are unchanged.
- `And`/`Or` boolean dispatch now refines lookup with conservative-superset child-shape buckets: it gates only impossible candidate families before exact guard checks, so admissibility (`ClassDispatch*`, `class_law_rule_guard`, typed/effect/scope constraints) and fixed-point cost/policy behavior remain unchanged.
