#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

const FRAME_TIMEOUT_MS = 5000;

function envFlag(name, defaultValue = false) {
  const raw = Deno.env.get(name);
  if (raw === undefined || raw === null || raw.length === 0) {
    return defaultValue;
  }
  const normalized = raw.toLowerCase();
  return normalized === "1" || normalized === "true" || normalized === "yes";
}

const REQUIRE_CORE_BACKENDS = envFlag("CLAPSE_EXPECT_CORE_LSP_BACKENDS", false);

function getWasmPath() {
  const candidates = [
    Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "",
    "out/clapse_compiler.wasm",
  ];
  for (const candidate of candidates) {
    if (candidate.length === 0) continue;
    try {
      Deno.statSync(candidate);
      return candidate;
    } catch {
      // keep searching
    }
  }
  throw new Error(
    "wasm LSP mode requires CLAPSE_COMPILER_WASM_PATH or out/clapse_compiler.wasm",
  );
}

function encodeMessage(payload) {
  const body = textEncoder.encode(JSON.stringify(payload));
  const header = textEncoder.encode(`Content-Length: ${body.length}\r\n\r\n`);
  const out = new Uint8Array(header.length + body.length);
  out.set(header, 0);
  out.set(body, header.length);
  return out;
}

function parseContentLength(header) {
  const lines = header.split("\r\n");
  for (const line of lines) {
    const match = line.match(/^Content-Length:\s*(\d+)$/i);
    if (match) {
      return Number(match[1]);
    }
  }
  return -1;
}

function findHeaderEnd(bytes) {
  for (let i = 0; i + 3 < bytes.length; i += 1) {
    if (
      bytes[i] === 13 &&
      bytes[i + 1] === 10 &&
      bytes[i + 2] === 13 &&
      bytes[i + 3] === 10
    ) {
      return i;
    }
  }
  return -1;
}

function extractHoverText(response) {
  if (response === null || response === undefined) {
    return null;
  }
  if (typeof response.contents === "string") {
    return response.contents;
  }
  if (typeof response.contents?.value === "string") {
    return response.contents.value;
  }
  if (Array.isArray(response.contents)) {
    return response.contents
      .map((entry) => {
        if (typeof entry === "string") return entry;
        if (entry && typeof entry.value === "string") return entry.value;
        return "";
      })
      .join(" ");
  }
  return JSON.stringify(response);
}

function diagnosticsExpectation(scenario) {
  const exact = Number.isFinite(Number(scenario?.expectedDiagnostics));
  const atLeast = Number.isFinite(Number(scenario?.expectedDiagnosticsAtLeast));
  if (atLeast) {
    return { expected: Number(scenario.expectedDiagnosticsAtLeast), exact: false };
  }
  return { expected: exact ? Number(scenario.expectedDiagnostics) : 0, exact: true };
}

function hoverExpectationPass(actual, expectation) {
  if (expectation === null) {
    return actual === null;
  }
  if (
    expectation && typeof expectation === "object" &&
    typeof expectation.contains === "string"
  ) {
    const text = extractHoverText(actual);
    return text !== null && text.includes(expectation.contains);
  }
  return false;
}

function definitionExpectationPass(actual, expectation, uri) {
  if (expectation === undefined) {
    return true;
  }
  if (expectation === null) {
    return !Array.isArray(actual) || actual.length === 0;
  }
  if (!Array.isArray(actual) || actual.length === 0) {
    return false;
  }
  const item = actual[0];
  const expectedLine = Number(expectation.line ?? NaN);
  const expectedCharacter = Number(expectation.character ?? NaN);
  const actualStart = item?.range?.start ?? {};
  const expectedUri = expectation.uri;
  if (
    typeof expectedUri === "string" && typeof item?.uri === "string" &&
    item.uri !== expectedUri && item.uri !== uri
  ) {
    return false;
  }
  return (
    Number.isFinite(expectedLine) &&
    Number.isFinite(expectedCharacter) &&
    actualStart.line === expectedLine &&
    actualStart.character === expectedCharacter
  );
}

function renameExpectationPass(actual, expectation, uri) {
  if (expectation === undefined) {
    return true;
  }
  if (expectation === null) {
    return actual === null;
  }
  if (actual === null || typeof actual !== "object") {
    return false;
  }
  const edits = actual?.changes?.[uri];
  if (!Array.isArray(edits)) {
    return false;
  }
  const expectedCount = Number(expectation.expectedEditCount);
  if (Number.isFinite(expectedCount) && edits.length !== expectedCount) {
    return false;
  }
  if (typeof expectation.newName === "string" && expectation.newName.length > 0) {
    return edits.every((edit) => String(edit?.newText ?? "") === expectation.newName);
  }
  return true;
}

function codeActionExpectationPass(actual, expectation) {
  if (expectation === undefined) {
    return true;
  }
  if (!Array.isArray(actual)) {
    return false;
  }
  if (expectation === null) {
    return actual.length === 0;
  }
  if (
    typeof expectation.exactCount === "number" &&
    actual.length !== expectation.exactCount
  ) {
    return false;
  }
  if (
    typeof expectation.minCount === "number" &&
    actual.length < expectation.minCount
  ) {
    return false;
  }
  if (typeof expectation.titleContains === "string") {
    return actual.some((action) =>
      String(action?.title ?? "").includes(expectation.titleContains)
    );
  }
  if (typeof expectation.title === "string") {
    return actual.some((action) => String(action?.title ?? "") === expectation.title);
  }
  return true;
}

function backendExpectationPass(actual, expectation) {
  if (expectation === undefined) {
    return true;
  }
  const resolved = typeof actual === "string" ? actual : "js";
  return resolved === expectation;
}

function prepareRenameExpectationPass(actual, expectation) {
  if (expectation === undefined) {
    return true;
  }
  if (expectation === null) {
    return actual === null;
  }
  if (actual === null || typeof actual !== "object") {
    return false;
  }
  const start = actual?.range?.start ?? {};
  const expectedLine = Number(expectation.line ?? NaN);
  const expectedCharacter = Number(expectation.character ?? NaN);
  if (
    Number.isFinite(expectedLine) &&
    Number.isFinite(expectedCharacter) &&
    (start.line !== expectedLine || start.character !== expectedCharacter)
  ) {
    return false;
  }
  if (typeof expectation.placeholder === "string") {
    return String(actual?.placeholder ?? "") === expectation.placeholder;
  }
  return true;
}

function referencesExpectationPass(actual, expectation, uri) {
  if (expectation === undefined) {
    return true;
  }
  if (!Array.isArray(actual)) {
    return false;
  }
  if (expectation === null) {
    return actual.length === 0;
  }
  if (typeof expectation.expectedCount === "number" && actual.length !== expectation.expectedCount) {
    return false;
  }
  if (typeof expectation.minCount === "number" && actual.length < expectation.minCount) {
    return false;
  }
  if (expectation.sameUri === true) {
    return actual.every((location) => String(location?.uri ?? "") === uri);
  }
  return true;
}

function documentSymbolsExpectationPass(actual, expectation) {
  if (expectation === undefined) {
    return true;
  }
  if (!Array.isArray(actual)) {
    return false;
  }
  if (expectation === null) {
    return actual.length === 0;
  }
  if (typeof expectation.exactCount === "number" && actual.length !== expectation.exactCount) {
    return false;
  }
  if (typeof expectation.minCount === "number" && actual.length < expectation.minCount) {
    return false;
  }
  if (typeof expectation.containsName === "string") {
    return actual.some((item) => String(item?.name ?? "") === expectation.containsName);
  }
  return true;
}

function formattingExpectationPass(actual, expectation, source) {
  if (expectation === undefined) {
    return true;
  }
  if (!Array.isArray(actual)) {
    return false;
  }
  if (expectation === null) {
    return actual.length === 0;
  }
  if (
    typeof expectation.expectedEditCount === "number" &&
    actual.length !== expectation.expectedEditCount
  ) {
    return false;
  }
  if (expectation.sameAsSource === true) {
    return actual.length === 1 && String(actual[0]?.newText ?? "") === source;
  }
  if (typeof expectation.exactText === "string") {
    return actual.length > 0 && String(actual[0]?.newText ?? "") === expectation.exactText;
  }
  if (typeof expectation.contains === "string") {
    return actual.some((edit) =>
      String(edit?.newText ?? "").includes(expectation.contains)
    );
  }
  return true;
}

async function run() {
  const fixturePath = "examples/lsp_wasm_fixtures.json";
  const fixtureText = await Deno.readTextFile(fixturePath);
  const fixture = JSON.parse(fixtureText);
  const scenarios = Array.isArray(fixture?.scenarios)
    ? fixture.scenarios
    : [];
  if (scenarios.length === 0) {
    throw new Error(`no scenarios found in ${fixturePath}`);
  }

  const wasmPath = getWasmPath();
  const outDir = "out/lsp-wasm-fixtures";
  const sourceDir = `${outDir}/sources`;
  await Deno.mkdir(sourceDir, { recursive: true });

  const proc = new Deno.Command("deno", {
    args: ["run", "-A", "scripts/lsp-wasm.mjs"],
    env: { ...Deno.env.toObject(), CLAPSE_COMPILER_WASM_PATH: wasmPath },
    stdin: "piped",
    stdout: "piped",
    stderr: "piped",
  }).spawn();

  if (proc.stdin === null || proc.stdout === null) {
    throw new Error("failed to start lsp process stdin/stdout");
  }

  const writer = proc.stdin.getWriter();
  const reader = proc.stdout.getReader();

  let nextId = 1;
  let buffer = new Uint8Array(0);

  const chunk = new Uint8Array(16 * 1024);
  const pendingRequests = new Map();
  const diagnosticsByUri = new Map();
  const diagnosticsWaiters = new Map();

  const parseLoop = (async () => {
    try {
      while (true) {
        const { value, done } = await reader.read();
        if (done) {
          break;
        }

        const next = new Uint8Array(buffer.length + value.length);
        next.set(buffer, 0);
        next.set(value, buffer.length);
        buffer = next;

        while (true) {
          const headerEnd = findHeaderEnd(buffer);
          if (headerEnd < 0) {
            break;
          }

          const header = textDecoder.decode(buffer.slice(0, headerEnd));
          const contentLength = parseContentLength(header);
          if (contentLength < 0) {
            throw new Error("invalid LSP header: missing Content-Length");
          }

          const bodyStart = headerEnd + 4;
          const bodyEnd = bodyStart + contentLength;
          if (buffer.length < bodyEnd) {
            break;
          }

          const body = buffer.slice(bodyStart, bodyEnd);
          buffer = buffer.slice(bodyEnd);

          const message = JSON.parse(textDecoder.decode(body));
          const id = message?.id;

          if (message?.method === "textDocument/publishDiagnostics") {
            const params = message?.params ?? {};
            const uri = String(params.uri ?? "");
            const diagnostics = Array.isArray(params.diagnostics)
              ? params.diagnostics
              : [];
            diagnosticsByUri.set(uri, diagnostics);

            const waiters = diagnosticsWaiters.get(uri);
            if (Array.isArray(waiters) && waiters.length > 0) {
              const waiter = waiters.shift();
              waiter.resolve(diagnostics);
              if (waiters.length === 0) {
                diagnosticsWaiters.delete(uri);
              }
            }
            continue;
          }

          if (id !== undefined) {
            const pending = pendingRequests.get(id);
            if (!pending) {
              continue;
            }
            pendingRequests.delete(id);
            if (message.error) {
              pending.reject(message.error.message ?? "LSP request failed");
            } else {
              pending.resolve(message.result ?? null);
            }
          }
        }
      }
    } catch (err) {
      for (const entry of pendingRequests.values()) {
        entry.reject(err);
      }
      pendingRequests.clear();
      for (const waiters of diagnosticsWaiters.values()) {
        for (const waiter of waiters) {
          waiter.reject(err);
        }
      }
      diagnosticsWaiters.clear();
      throw err;
    }
  })();

  function requestTimeout(ms) {
    return new Promise((_, reject) => {
      setTimeout(() => {
        reject(new Error(`request timeout after ${ms}ms`));
      }, ms);
    });
  }

  async function sendRequest(method, params) {
    const id = nextId;
    nextId += 1;
    const message = { jsonrpc: "2.0", id, method, params };
    await writer.write(encodeMessage(message));
    const reply = new Promise((resolve, reject) => {
      pendingRequests.set(id, { resolve, reject });
    });
    return Promise.race([reply, requestTimeout(FRAME_TIMEOUT_MS)]);
  }

  async function sendNotification(method, params) {
    const message = { jsonrpc: "2.0", method, params };
    await writer.write(encodeMessage(message));
  }

  function waitForDiagnostics(uri) {
    const pending = diagnosticsByUri.get(uri);
    if (pending !== undefined) {
      return Promise.resolve(pending);
    }

    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        const waiters = diagnosticsWaiters.get(uri) ?? [];
        const remaining = waiters.filter((w) => w.resolve !== resolve);
        if (remaining.length > 0) {
          diagnosticsWaiters.set(uri, remaining);
        } else {
          diagnosticsWaiters.delete(uri);
        }
        reject(new Error(`timed out waiting for diagnostics on ${uri}`));
      }, FRAME_TIMEOUT_MS);
      const waiter = {
        resolve: (diagnostics) => {
          clearTimeout(timer);
          resolve(diagnostics);
        },
        reject,
      };
      const waiters = diagnosticsWaiters.get(uri) ?? [];
      waiters.push(waiter);
      diagnosticsWaiters.set(uri, waiters);
    });
  }

  const scenarioResults = [];
  try {
    await sendRequest("initialize", {
      processId: Deno.pid,
      rootUri: null,
      capabilities: {
        textDocument: {
          hover: { dynamicRegistration: true },
          diagnostics: { refreshSupport: true },
        },
      },
      clientInfo: { name: "clapse-wasm-fixture-runner", version: "0.1.0" },
    });

    await sendNotification("initialized", {});

    for (let i = 0; i < scenarios.length; i += 1) {
      const scenario = scenarios[i];
      const name = String(scenario?.name ?? `scenario-${i + 1}`);
      const source = String(scenario?.source ?? "");
      const hoverPos = scenario?.hover ?? {};
      const line = Number(hoverPos.line ?? 0);
      const character = Number(hoverPos.character ?? 0);
      const hoverExpectation = scenario?.hoverExpectation ?? null;

      const { expected, exact } = diagnosticsExpectation(scenario);
      const definitionReq = scenario?.definition;
      const definitionExpectation = scenario?.definitionExpectation;
      const prepareRenameReq = scenario?.prepareRename;
      const prepareRenameExpectation = scenario?.prepareRenameExpectation;
      const renameReq = scenario?.rename;
      const renameExpectation = scenario?.renameExpectation;
      const expectedBackends = REQUIRE_CORE_BACKENDS
        ? scenario?.expectedBackends
        : undefined;
      const referencesReq = scenario?.references;
      const referencesExpectation = scenario?.referencesExpectation;
      const documentSymbolsReq = scenario?.documentSymbols;
      const documentSymbolsExpectation = scenario?.documentSymbolsExpectation;
      const codeActionReq = scenario?.codeAction;
      const codeActionExpectation = scenario?.codeActionExpectation;
      const formattingReq = scenario?.formatting;
      const formattingExpectation = scenario?.formattingExpectation;
      const sourceFile = String(scenario?.sourceFile ?? `main.clapse`);
      const projectConfig = scenario?.projectConfig ?? null;
      const projectFiles = scenario?.projectFiles;
      const projectDir = await Deno.makeTempDir({
        dir: sourceDir,
        prefix: `fixture-${i + 1}-`,
      });
      const uri = `${projectDir}/${sourceFile}`;
      const parentEnd = uri.lastIndexOf("/");
      if (parentEnd >= 0) {
        await Deno.mkdir(uri.slice(0, parentEnd), { recursive: true });
      }
      if (projectConfig !== null) {
        await Deno.writeTextFile(
          `${projectDir}/clapse.json`,
          JSON.stringify(projectConfig, null, 2),
        );
      }
      if (
        projectFiles !== null &&
        typeof projectFiles === "object" &&
        !Array.isArray(projectFiles)
      ) {
        for (const [name, content] of Object.entries(projectFiles)) {
          const filePath = `${projectDir}/${String(name)}`;
          const parentEnd = filePath.lastIndexOf("/");
          if (parentEnd >= 0) {
            await Deno.mkdir(filePath.slice(0, parentEnd), { recursive: true });
          }
          await Deno.writeTextFile(filePath, String(content ?? ""));
        }
      }
      await Deno.writeTextFile(uri, source);
      diagnosticsByUri.delete(uri);

      const diagPromise = waitForDiagnostics(uri);
      await sendNotification("textDocument/didOpen", {
        textDocument: {
          uri,
          languageId: "clapse",
          version: 1,
          text: source,
        },
      });
      const diagnostics = await diagPromise;
      const actualDiagnosticCount = Array.isArray(diagnostics) ? diagnostics.length : 0;

      const hoverResp = await sendRequest("textDocument/hover", {
        textDocument: { uri },
        position: { line, character },
      });

      const diagnosticsPass = exact
        ? actualDiagnosticCount === expected
        : actualDiagnosticCount >= expected;
      const hoverPass = hoverExpectationPass(hoverResp, hoverExpectation);
      const hoverBackend = hoverResp?.backend ?? "js";
      const hoverBackendPass = backendExpectationPass(hoverBackend, expectedBackends?.hover);

      let definitionResp = null;
      let definitionPass = true;
      if (definitionReq && typeof definitionReq === "object") {
        const defLine = Number(definitionReq?.line ?? 0);
        const defCharacter = Number(definitionReq?.character ?? 0);
        definitionResp = await sendRequest("textDocument/definition", {
          textDocument: { uri },
          position: { line: defLine, character: defCharacter },
        });
        definitionPass = definitionExpectationPass(
          definitionResp,
          definitionExpectation,
          uri,
        );
      }
      const definitionBackend = definitionResp?.backend ?? "js";
      const definitionBackendPass = backendExpectationPass(definitionBackend, expectedBackends?.definition);

      let renameResp = null;
      let renamePass = true;
      let prepareRenameResp = null;
      let prepareRenamePass = true;
      if (prepareRenameReq && typeof prepareRenameReq === "object") {
        const renameLine = Number(prepareRenameReq?.line ?? 0);
        const renameCharacter = Number(prepareRenameReq?.character ?? 0);
        prepareRenameResp = await sendRequest("textDocument/prepareRename", {
          textDocument: { uri },
          position: { line: renameLine, character: renameCharacter },
        });
        prepareRenamePass = prepareRenameExpectationPass(
          prepareRenameResp,
          prepareRenameExpectation,
        );
      }

      if (renameReq && typeof renameReq === "object") {
        const renameLine = Number(renameReq?.line ?? 0);
        const renameCharacter = Number(renameReq?.character ?? 0);
        const newName = String(renameReq?.newName ?? "");
        renameResp = await sendRequest("textDocument/rename", {
          textDocument: { uri },
          position: { line: renameLine, character: renameCharacter },
          newName,
        });
        renamePass = renameExpectationPass(renameResp, renameExpectation, uri);
      }

      let referencesResp = null;
      let referencesPass = true;
      if (referencesReq && typeof referencesReq === "object") {
        const referencesLine = Number(referencesReq?.line ?? 0);
        const referencesCharacter = Number(referencesReq?.character ?? 0);
        const includeDeclaration = referencesReq?.includeDeclaration !== false;
        referencesResp = await sendRequest("textDocument/references", {
          textDocument: { uri },
          position: { line: referencesLine, character: referencesCharacter },
          context: { includeDeclaration },
        });
        referencesPass = referencesExpectationPass(
          referencesResp,
          referencesExpectation,
          uri,
        );
      }

      let documentSymbolsResp = null;
      let documentSymbolsPass = true;
      if (documentSymbolsReq === true || documentSymbolsExpectation !== undefined) {
        documentSymbolsResp = await sendRequest("textDocument/documentSymbol", {
          textDocument: { uri },
        });
        documentSymbolsPass = documentSymbolsExpectationPass(
          documentSymbolsResp,
          documentSymbolsExpectation,
        );
      }

      let codeActionResp = null;
      let codeActionPass = true;
      if (codeActionReq && typeof codeActionReq === "object") {
        const codeActionLine = Number(codeActionReq?.line ?? 0);
        const codeActionCharacter = Number(codeActionReq?.character ?? 0);
        codeActionResp = await sendRequest("textDocument/codeAction", {
          textDocument: { uri },
          range: {
            start: { line: codeActionLine, character: codeActionCharacter },
            end: { line: codeActionLine, character: codeActionCharacter },
          },
          context: {
            diagnostics,
            only: ["quickfix"],
          },
        });
        codeActionPass = codeActionExpectationPass(codeActionResp, codeActionExpectation);
      }

      let formattingResp = null;
      let formattingPass = true;
      if (formattingReq === true || formattingExpectation !== undefined) {
        formattingResp = await sendRequest("textDocument/formatting", {
          textDocument: { uri },
          options: { tabSize: 2, insertSpaces: true },
        });
        formattingPass = formattingExpectationPass(
          formattingResp,
          formattingExpectation,
          source,
        );
      }

      const pass = diagnosticsPass &&
        hoverPass &&
        hoverBackendPass &&
        definitionPass &&
        definitionBackendPass &&
        prepareRenamePass &&
        renamePass &&
        referencesPass &&
        documentSymbolsPass &&
        codeActionPass &&
        formattingPass;

      const result = {
        name,
        source,
        uri,
        diagnostics: {
          expected,
          expectedExact: exact,
          actual: actualDiagnosticCount,
          passed: diagnosticsPass,
        },
        hover: {
          expectation: hoverExpectation,
          actual: hoverResp,
          backend: hoverBackend,
          backendPass: hoverBackendPass,
          passed: hoverPass,
        },
        definition: {
          request: definitionReq ?? null,
          expectation: definitionExpectation ?? null,
          actual: definitionResp,
          backend: definitionBackend,
          backendPass: definitionBackendPass,
          passed: definitionPass,
        },
        rename: {
          request: prepareRenameReq ?? null,
          expectation: prepareRenameExpectation ?? null,
          actual: prepareRenameResp,
          passed: prepareRenamePass,
        },
        renameEdits: {
          request: renameReq ?? null,
          expectation: renameExpectation ?? null,
          actual: renameResp,
          passed: renamePass,
        },
        references: {
          request: referencesReq ?? null,
          expectation: referencesExpectation ?? null,
          actual: referencesResp,
          passed: referencesPass,
        },
        documentSymbols: {
          request: documentSymbolsReq ?? null,
          expectation: documentSymbolsExpectation ?? null,
          actual: documentSymbolsResp,
          passed: documentSymbolsPass,
        },
        codeAction: {
          request: codeActionReq ?? null,
          expectation: codeActionExpectation ?? null,
          actual: codeActionResp,
          passed: codeActionPass,
        },
        formatting: {
          request: formattingReq ?? null,
          expectation: formattingExpectation ?? null,
          actual: formattingResp,
          passed: formattingPass,
        },
        pass,
      };

      if (!pass) {
        const failures = [];
        if (!diagnosticsPass) {
          failures.push(
            exact
              ? `diagnostics count mismatch: expected ${expected}, got ${actualDiagnosticCount}`
              : `diagnostics count mismatch: expected >= ${expected}, got ${actualDiagnosticCount}`,
          );
        }
        if (!hoverPass) {
          failures.push("hover expectation mismatch");
        }
        if (!hoverBackendPass) {
          failures.push(`hover backend mismatch: expected ${expectedBackends?.hover}, got ${hoverBackend}`);
        }
        if (!definitionPass) {
          failures.push("definition expectation mismatch");
        }
        if (!definitionBackendPass) {
          failures.push(`definition backend mismatch: expected ${expectedBackends?.definition}, got ${definitionBackend}`);
        }
        if (!renamePass) {
          failures.push("rename expectation mismatch");
        }
        if (!prepareRenamePass) {
          failures.push("prepare rename expectation mismatch");
        }
        if (!referencesPass) {
          failures.push("references expectation mismatch");
        }
        if (!documentSymbolsPass) {
          failures.push("document symbol expectation mismatch");
        }
        if (!codeActionPass) {
          failures.push("code action expectation mismatch");
        }
        if (!formattingPass) {
          failures.push("formatting expectation mismatch");
        }
        result.failure = {
          message: failures.join("; "),
        };
      }

      scenarioResults.push(result);
    }

    await sendRequest("shutdown", null);
    await sendNotification("exit", null);
  } finally {
    await writer.close();
  }

  const outPath = `${outDir}/report.json`;
  const allPassed = scenarioResults.every((result) => result.pass);
  const report = {
    status: allPassed ? "PASS" : "FAIL",
    wasmPath,
    scenarios: scenarioResults,
  };
  await Deno.writeTextFile(outPath, JSON.stringify(report, null, 2));

  const status = await proc.status;
  if (!status.success) {
    throw new Error(`lsp-wasm process exited with ${status.code ?? 1}`);
  }
  await parseLoop;

  if (!allPassed) {
    console.error(`LSP fixtures: FAIL (${scenarioResults.filter((r) => !r.pass).length}/${scenarioResults.length})`);
    for (const result of scenarioResults) {
      const diagRule = result.diagnostics.expectedExact
        ? `diagnostics=${result.diagnostics.expected}`
        : `diagnostics>=${result.diagnostics.expected}`;
      const statusLabel = result.pass ? "PASS" : "FAIL";
      console.error(`${statusLabel} ${result.name} (${diagRule})`);
      if (result.failure) {
        console.error(`  ${result.failure.message}`);
      }
    }
    console.log(`report: ${outPath}`);
    Deno.exit(1);
  }

  console.log(`LSP fixtures: PASS (${scenarioResults.length}/${scenarioResults.length})`);
  for (const result of scenarioResults) {
    const diagRule = result.diagnostics.expectedExact
      ? `diagnostics=${result.diagnostics.expected}`
      : `diagnostics>=${result.diagnostics.expected}`;
    console.log(`PASS ${result.name} (${diagRule})`);
  }
  console.log(`report: ${outPath}`);
}

await run().catch(failWithError);
