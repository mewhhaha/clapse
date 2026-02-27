# clapse gh-pages explorer

This branch is an artifact-only GitHub Pages app.

It provides:

- release-aware compiler selection from GitHub Releases,
- a browser pipeline for `code -> IR -> wasm`,
- side-by-side version comparison,
- a wasm export REPL runner.

## Files

- `index.html`
- `app.js`
- `styles.css`
- `docs/SKILL.md`

## Run Locally

```bash
deno serve --allow-read --port 4173 .
```

Open `http://localhost:4173/`.
