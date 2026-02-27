import { define } from "../utils.ts";

export default define.page(function Home() {
  return (
    <>
      <main class="page">
        <header class="top-strip panel">
          <div class="header-controls">
            <label class="control">
              <input id="auto-run" type="checkbox" checked />
              <span>Auto-run</span>
            </label>
            <button id="run-button" class="toolbar-button" type="button">
              Run (Ctrl + Enter)
            </button>
            <button id="format-button" class="toolbar-button" type="button">
              Format (Ctrl + S)
            </button>
            <label class="control program-control">
              <span>Example</span>
              <select id="program-select">
                <option value="">Example Programs</option>
              </select>
            </label>
            <label class="control release-control">
              <span>Release</span>
              <select id="release-select" />
            </label>
            <a
              id="release-link"
              class="header-link"
              href="https://github.com/mewhhaha/clapse/releases"
              target="_blank"
              rel="noopener noreferrer"
            >
              Open Selected Release
            </a>
            <span id="release-status" class="status">
              Loading release metadata...
            </span>
          </div>
        </header>

        <section class="workspace">
          <article class="panel editor-pane">
            <h2>Source</h2>
            <div class="pane-body source-editor-wrap">
              <pre
                id="source-highlight"
                class="source-highlight"
                aria-hidden="true"
              >
                <code id="source-highlight-code"></code>
              </pre>
              <textarea
                id="source-code"
                class="source-input"
                spellcheck={false}
                placeholder="Write Clapse code here..."
              />
            </div>
          </article>

          <aside class="panel right-pane">
            <nav class="tabs" role="tablist" aria-label="Explorer outputs">
              <button
                type="button"
                class="tab-button"
                data-tab-target="ir"
                aria-selected="false"
              >
                IR
              </button>
              <button
                type="button"
                class="tab-button is-active"
                data-tab-target="compile"
                aria-selected="true"
              >
                Compile
              </button>
              <button
                type="button"
                class="tab-button"
                data-tab-target="problems"
                aria-selected="false"
              >
                Problems
              </button>
              <button
                type="button"
                class="tab-button"
                data-tab-target="settings"
                aria-selected="false"
              >
                Settings
              </button>
            </nav>

            <section class="tab-panels">
              <section class="tab-panel" data-tab-panel="ir" hidden>
                <pre id="ir-output" class="result-output">
                  (IR appears after release load.)
                </pre>
              </section>

              <section class="tab-panel is-active" data-tab-panel="compile">
                <pre id="compile-output" class="result-output">
                  (Compile output appears after release load.)
                </pre>
                <a id="wasm-download" href="#" download hidden>
                  Download Compiled Wasm
                </a>
              </section>

              <section class="tab-panel" data-tab-panel="problems" hidden>
                <pre id="problems-output" class="result-output">
                  No problems.
                </pre>
              </section>

              <section
                class="tab-panel settings-panel"
                data-tab-panel="settings"
                hidden
              >
                <label class="setting-item">
                  <input id="settings-auto-run" type="checkbox" checked />
                  <span>Auto-run compile on edit</span>
                </label>
                <label class="setting-item">
                  <input id="settings-format-on-run" type="checkbox" />
                  <span>Format before run</span>
                </label>
                <p id="settings-highlight-note" class="settings-note">
                  Release metadata is loaded from GitHub Releases and compiler
                  wasm is fetched from release assets.
                </p>
              </section>
            </section>
          </aside>
        </section>
      </main>

      <script type="module" src="/app.js" />
    </>
  );
});
