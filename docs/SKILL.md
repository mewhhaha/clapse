---
name: gh-pages-explorer
description: Maintain the artifact-only GitHub Pages explorer for code -> IR -> wasm on selected releases.
---

# Clapse GH Pages Skill

## Purpose

This branch hosts a client-only explorer that runs the Clapse compiler wasm
directly in the browser.

Main workflow: `code -> IR -> wasm`.

## Canonical Entry Points

- `routes/index.tsx`: page structure and controls.
- `routes/_app.tsx`: document shell and global metadata.
- `static/app.js`: release loading, compiler asset resolution, compiler calls,
  and pane updates.
- `static/styles.css`: visual system and responsive layout.
- `main.ts`: Fresh app bootstrap.
- `deno.json`: Fresh/vite tasks and imports.

When behavior changes, update this file first, then update code.

## Release Source Contract

- Release dropdown data comes from GitHub Releases metadata:
  `https://api.github.com/repos/mewhhaha/clapse/releases`.
- Selected release pages should open at:
  `https://github.com/mewhhaha/clapse/releases`.
- Compiler wasm loading prefers release assets (`clapse_compiler.wasm`) via
  `browser_download_url`.
- If asset download fails, loader falls back to tag path
  `artifacts/latest/clapse_compiler.wasm` for that selected release tag.

## IR Contract

- IR output uses `selfhost-artifacts`.
- The explorer surfaces:
  - `lowered_ir.txt`
  - `collapsed_ir.txt`

If a version omits these files or command support, it is treated as unsupported
for this page.

## Layout Contract

- Header has a single release dropdown plus compile action.
- Main area has three side-by-side panes:
  - `Code` (editable)
  - `IR` (read-only)
  - `Wasm` (read-only)
- First two panes are horizontally resizable on desktop.

## Local Preview

Run the Fresh dev server:

```bash
deno task dev
```

Then open the printed local URL.
