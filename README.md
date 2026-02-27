# clapse gh-pages explorer

This branch is an artifact-only GitHub Pages app.

It provides:

- release-aware compiler selection from GitHub Releases,
- a browser pipeline for `code -> IR -> wasm`,
- a single-header release dropdown,
- three side-by-side panes where only `Code` is editable.

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
