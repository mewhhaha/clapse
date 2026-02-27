import { define } from "../utils.ts";

export default define.page(function Home() {
  return (
    <>
      <main class="page">
        <header class="hero panel">
          <p class="eyebrow">Artifact-only GitHub Pages</p>
          <h1>Clapse Explorer</h1>
          <p class="lede">
            Imagine Godbolt mixed with GitHub releases: edit code and inspect
            <code>code -&gt; IR -&gt; wasm</code> in real time.
          </p>
          <div class="header-controls">
            <label class="control">
              Release
              <select id="release-select" />
            </label>
            <span class="auto-chip">Auto compile on edit</span>
            <a
              id="release-link"
              class="header-link"
              href="https://github.com/mewhhaha/clapse/releases"
              target="_blank"
              rel="noopener noreferrer"
            >
              Open Selected Release
            </a>
          </div>
          <p id="release-status" class="status">Loading release metadata...</p>
        </header>

        <section class="panes">
          <article class="panel pane resize-left">
            <h2>Code</h2>
            <textarea
              id="source-code"
              class="pane-body"
              spellcheck={false}
              placeholder="Write Clapse code here..."
            />
          </article>

          <article class="panel pane resize-middle">
            <h2>IR</h2>
            <pre id="ir-output" class="pane-body result-output">
              (IR appears after release load.)
            </pre>
          </article>

          <article class="panel pane">
            <h2>Wasm</h2>
            <pre id="wasm-output" class="pane-body result-output">
              (Wasm summary appears after release load.)
            </pre>
            <a id="wasm-download" href="#" download hidden>
              Download Compiled Wasm
            </a>
          </article>
        </section>
      </main>

      <script type="module" src="/app.js" />
    </>
  );
});
