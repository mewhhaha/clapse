# clapse gh-pages explorer

This branch is an artifact-only GitHub Pages app.

It provides:

- release-aware compiler selection from GitHub Releases,
- a browser pipeline for `code -> IR -> wasm`,
- auto-compile on code or release changes (no compile button),
- a sticky top header with release controls and release link,
- three side-by-side panes that fill the viewport (`Code` editable; `IR`/`Wasm`
  read-only).
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
