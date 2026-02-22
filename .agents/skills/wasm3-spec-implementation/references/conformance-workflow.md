# Conformance Workflow

## Primary Conformance Source

- WebAssembly testsuite repository: https://github.com/WebAssembly/testsuite

Use spec tests as the primary oracle for correctness.

## Triage Loop

For every failing test:

1. classify failure:
   - decode/parsing
   - validation/type-checking
   - runtime semantics
   - host boundary
2. map failure to a Core spec chapter URL
3. write a focused regression test in your project
4. patch the engine/compiler
5. rerun baseline + targeted tests

## Test Matrix Strategy

Maintain separate lanes:

- `core-baseline`: MVP features only
- `proposal-*`: one lane per optional feature
- `embed-*`: host/API integration behavior

Never hide baseline regressions behind proposal flags.

## Failure Logging Format

For each unresolved failure, log:

- failing test id/name
- current behavior
- expected behavior
- linked spec URL
- status (`todo`, `blocked`, `fixed`)

## Regression Rules

- Add tests before refactors in parser/validator/executor hot paths.
- Keep semantically distinct fixes separate from performance-only changes.
- Reject "fixes" that only satisfy one test but violate spec text elsewhere.

## Completion Criteria

Treat a feature as complete only when:

1. relevant spec tests pass
2. project-specific regression tests pass
3. unsupported behavior is documented explicitly
