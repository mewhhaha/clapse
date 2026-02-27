---
name: gh-pages-explorer
description: Maintain the artifact-only GitHub Pages explorer and REPL for Clapse release comparison.
---

# Clapse GH Pages Skill

## Purpose

This branch hosts a client-only explorer and REPL that runs the Clapse compiler wasm directly in the browser.

Main workflow: `code -> IR -> wasm -> REPL`.

## Canonical Entry Points

- `index.html`: page structure and controls.
- `app.js`: release loading, compatibility probing, compiler calls, compare logic, REPL execution.
- `styles.css`: visual system and responsive layout.

When behavior changes, update this file first, then update code.

## Release Source Contract

- Versions come from GitHub Releases metadata:
  `https://api.github.com/repos/mewhhaha/clapse/releases`.
- Wasm bytes are fetched by tag from:
  `https://raw.githubusercontent.com/mewhhaha/clapse/<tag>/artifacts/latest/clapse_compiler.wasm`.
- Supported versions are filtered in-browser:
  - must instantiate in native mode (bridge artifacts are rejected),
  - must export `clapse_run`,
  - must respond to both `compile` and `selfhost-artifacts` commands with structured JSON.

## IR Contract

- IR output uses `selfhost-artifacts`.
- The explorer surfaces:
  - `lowered_ir.txt`
  - `collapsed_ir.txt`

If a version omits these files or command support, it is treated as unsupported for this page.

## REPL Contract

- REPL invokes the selected export from compiled wasm.
- Args are parsed as integers and encoded as tagged ints before invocation.
- Result rendering supports:
  - tagged ints,
  - slice-like text handles when decodable,
  - raw fallback handles.

## Local Preview

Serve static files from repo root, for example:

```bash
deno serve --allow-read --port 4173 .
```

Then open `http://localhost:4173/`.
