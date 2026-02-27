# clapse gh-pages explorer

This branch is an artifact-only GitHub Pages app.

It provides:

- release support filtered to tags `>= v0.1.0.12` that include compiler and
  prelude assets,
- release-aware compiler selection from GitHub Releases,
- a browser pipeline for `code -> IR -> wasm`,
- a compact sticky control strip with `Auto-run`, `Run`, `Format`, release
  picker, example-program picker, and release link,
- a two-column workspace with tab groups on both sides,
- left-side tabs for `Code` (editable) and `Prelude` (read-only from release
  assets),
- right-side tabs for `IR`, `Compile`, `Problems`, and `Settings`,
- live source syntax highlighting in the editor overlay,
- current highlight mode uses an in-browser tokenizer (release assets currently
  do not include a Tree-sitter grammar wasm),
- compile includes selected release `prelude.clapse` plus your code source,
- auto-compile on edits and release changes when auto-run is enabled.
- choosing an example program replaces the source editor text.
- built with Deno Fresh.

## Files

- `routes/index.tsx`
- `static/app.js`
- `static/styles.css`
- `main.ts`
- `deno.json`
- `docs/SKILL.md`

## Run Locally

```bash
deno task dev
```

Then open the printed local URL.
