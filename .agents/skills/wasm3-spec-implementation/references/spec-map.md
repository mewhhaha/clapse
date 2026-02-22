# WebAssembly Spec Map

Last checked: 2026-02-22.

## Canonical Sources

- Core specs index: https://webassembly.org/specs/
- Core specification release 3.0 (2026-02-21): https://webassembly.github.io/spec/core/
- JavaScript API 2.0 (Editors' Draft): https://webassembly.github.io/spec/js-api/
- Web API 1.0 (Editors' Draft): https://webassembly.github.io/spec/web-api/
- Feature/proposal status: https://webassembly.org/features/

Use this order:

1. Core specification
2. JS/Web API specifications (only for embedding behavior)
3. Feature status page (for planning feature flags)

## Task-to-Chapter Mapping

### Module decoding and binary reader

- Binary values and integer encodings:
  https://webassembly.github.io/spec/core/binary/values.html
- Binary module structure and sections:
  https://webassembly.github.io/spec/core/binary/modules.html
- Binary instruction opcodes/immediates:
  https://webassembly.github.io/spec/core/binary/instructions.html

### Text format and parser behavior

- Text values and lexical structure:
  https://webassembly.github.io/spec/core/text/values.html
- Text module grammar:
  https://webassembly.github.io/spec/core/text/modules.html
- Text instruction grammar:
  https://webassembly.github.io/spec/core/text/instructions.html

### Validation

- Module validation rules:
  https://webassembly.github.io/spec/core/valid/modules.html
- Instruction validation rules:
  https://webassembly.github.io/spec/core/valid/instructions.html
- Types and matching rules:
  https://webassembly.github.io/spec/core/valid/types.html

### Execution semantics

- Runtime structure (store/instances):
  https://webassembly.github.io/spec/core/exec/runtime.html
- Instruction execution steps:
  https://webassembly.github.io/spec/core/exec/instructions.html
- Module instantiation and invocation:
  https://webassembly.github.io/spec/core/exec/modules.html

### Embedding and host integration

- Core embedding overview:
  https://webassembly.github.io/spec/core/appendix/embedding.html
- JavaScript API behavior:
  https://webassembly.github.io/spec/js-api/
- Browser web embedding details:
  https://webassembly.github.io/spec/web-api/

## Recommended Implementation Order

1. Decoder for module sections and basic instruction stream
2. Validator with explicit value/control stack invariants
3. Interpreter for numeric/control/call/memory baseline instructions
4. Host imports/exports boundary and deterministic trap handling
5. Optional proposals behind explicit feature flags

## Spec-Trace Format

For each non-trivial change, record:

- spec chapter URL
- rule/algorithm being implemented
- behavior enforced
- test proving behavior
