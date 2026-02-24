#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

const FRAME_TIMEOUT_MS = 5000;

function getWasmPath() {
  const allowBridge =
    (Deno.env.get("CLAPSE_ALLOW_BRIDGE") ?? "").toLowerCase() === "1" ||
    (Deno.env.get("CLAPSE_ALLOW_BRIDGE") ?? "").toLowerCase() === "true";
  const candidates = [
    Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "",
    "out/clapse_compiler.wasm",
    ...(allowBridge ? ["out/clapse_compiler_bridge.wasm"] : []),
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
    "wasm LSP mode requires CLAPSE_COMPILER_WASM_PATH or out/clapse_compiler.wasm (bridge additionally requires CLAPSE_ALLOW_BRIDGE=1)",
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

      const uri = await Deno.makeTempFile({
        dir: sourceDir,
        prefix: `fixture-${i + 1}-`,
        suffix: ".clapse",
      });
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
      const pass = diagnosticsPass && hoverPass;

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
          passed: hoverPass,
        },
        pass,
      };

      if (!pass) {
        result.failure = {
          message: !diagnosticsPass
            ? exact
              ? `diagnostics count mismatch: expected ${expected}, got ${actualDiagnosticCount}`
              : `diagnostics count mismatch: expected >= ${expected}, got ${actualDiagnosticCount}`
            : "hover expectation mismatch",
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
