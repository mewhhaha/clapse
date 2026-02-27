# clapse gh-pages explorer

This branch is an artifact-only GitHub Pages app.

It provides:

- release-aware compiler selection from GitHub Releases,
- a browser pipeline for `code -> IR -> wasm`,
- a single-header release dropdown,
- three side-by-side panes where only `Code` is editable.
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
