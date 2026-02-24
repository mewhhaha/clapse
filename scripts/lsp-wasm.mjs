#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";
import {
  callCompilerWasm,
  validateCompilerWasmAbi,
} from "./wasm-compiler-abi.mjs";

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function getWasmPath() {
  const allowBridge =
    ((Deno.env.get("CLAPSE_ALLOW_BRIDGE") ?? "").toLowerCase() === "1") ||
    ((Deno.env.get("CLAPSE_ALLOW_BRIDGE") ?? "").toLowerCase() === "true");
  const candidates = [
    Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "",
    "out/clapse_compiler.wasm",
    ...(allowBridge ? ["out/clapse_compiler_bridge.wasm"] : []),
  ];
  for (const wasmPath of candidates) {
    if (wasmPath.length === 0) continue;
    try {
      Deno.statSync(wasmPath);
      return wasmPath;
    } catch {
      // keep searching
    }
  }
  throw new Error(
    "wasm LSP mode requires CLAPSE_COMPILER_WASM_PATH or out/clapse_compiler.wasm (bridge additionally requires CLAPSE_ALLOW_BRIDGE=1)",
  );
}

function parseLineError(message) {
  const m = message.match(/line\s+(\d+):\s*(.*)$/i);
  if (!m) return null;
  const line = Math.max(0, Number(m[1]) - 1);
  const msg = m[2] || message;
  return { line, msg };
}

function fullRangeForText(text) {
  const lines = text.split("\n");
  const endLine = Math.max(0, lines.length - 1);
  const endCharacter = lines.length === 0 ? 0 : lines[endLine].length;
  return {
    start: { line: 0, character: 0 },
    end: { line: endLine, character: endCharacter },
  };
}

async function formatSource(wasmPath, uri, source) {
  const response = await callCompilerWasm(wasmPath, {
    command: "format",
    mode: "stdout",
    input_path: uri,
    source,
  });
  if (
    !response || typeof response !== "object" || response.ok !== true ||
    typeof response.formatted !== "string"
  ) {
    const err = typeof response?.error === "string"
      ? response.error
      : "format failed";
    throw new Error(err);
  }
  return response.formatted;
}

async function compileDiagnostics(wasmPath, uri, source) {
  const response = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: uri,
    input_source: source,
  });
  if (response && typeof response === "object" && response.ok === true) {
    return [];
  }
  const message = response && typeof response.error === "string"
    ? response.error
    : "compile failed";
  const parsed = parseLineError(message);
  if (parsed) {
    return [{
      range: {
        start: { line: parsed.line, character: 0 },
        end: { line: parsed.line, character: 1 },
      },
      severity: 1,
      source: "clapse",
      message: parsed.msg,
    }];
  }
  return [{
    range: {
      start: { line: 0, character: 0 },
      end: { line: 0, character: 1 },
    },
    severity: 1,
    source: "clapse",
    message,
  }];
}

function encodeMessage(payload) {
  const body = encoder.encode(JSON.stringify(payload));
  const header = encoder.encode(`Content-Length: ${body.length}\r\n\r\n`);
  const out = new Uint8Array(header.length + body.length);
  out.set(header, 0);
  out.set(body, header.length);
  return out;
}

async function writeMessage(payload) {
  await Deno.stdout.write(encodeMessage(payload));
}

async function sendResponse(id, result) {
  await writeMessage({ jsonrpc: "2.0", id, result });
}

async function sendError(id, code, message) {
  await writeMessage({ jsonrpc: "2.0", id, error: { code, message } });
}

async function sendNotification(method, params) {
  await writeMessage({ jsonrpc: "2.0", method, params });
}

async function readMessages(onMessage) {
  let buffer = new Uint8Array(0);
  const chunk = new Uint8Array(16 * 1024);
  while (true) {
    const n = await Deno.stdin.read(chunk);
    if (n === null) break;
    const next = new Uint8Array(buffer.length + n);
    next.set(buffer, 0);
    next.set(chunk.slice(0, n), buffer.length);
    buffer = next;

    while (true) {
      const headerEnd = findHeaderEnd(buffer);
      if (headerEnd < 0) break;
      const headerBytes = buffer.slice(0, headerEnd);
      const header = decoder.decode(headerBytes);
      const len = parseContentLength(header);
      if (len < 0) {
        throw new Error("invalid LSP header: missing Content-Length");
      }
      const bodyStart = headerEnd + 4;
      const bodyEnd = bodyStart + len;
      if (buffer.length < bodyEnd) break;
      const body = buffer.slice(bodyStart, bodyEnd);
      buffer = buffer.slice(bodyEnd);
      const message = JSON.parse(decoder.decode(body));
      await onMessage(message);
    }
  }
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

function parseContentLength(header) {
  const lines = header.split("\r\n");
  for (const line of lines) {
    const m = line.match(/^Content-Length:\s*(\d+)$/i);
    if (m) return Number(m[1]);
  }
  return -1;
}

async function main() {
  const wasmPath = getWasmPath();
  await validateCompilerWasmAbi(wasmPath);
  const docs = new Map();
  let shutdownRequested = false;

  await readMessages(async (msg) => {
    const method = msg?.method;
    const id = msg?.id;

    try {
      if (method === "initialize") {
        await sendResponse(id, {
          capabilities: {
            textDocumentSync: 1,
            documentFormattingProvider: true,
            hoverProvider: false,
            inlayHintProvider: false,
          },
          serverInfo: { name: "clapse-wasm-lsp", version: "0.1.0" },
        });
        return;
      }
      if (method === "initialized") {
        return;
      }
      if (method === "shutdown") {
        shutdownRequested = true;
        await sendResponse(id, null);
        return;
      }
      if (method === "exit") {
        Deno.exit(shutdownRequested ? 0 : 1);
      }

      if (method === "textDocument/didOpen") {
        const uri = msg.params?.textDocument?.uri;
        const text = msg.params?.textDocument?.text ?? "";
        if (typeof uri === "string") {
          docs.set(uri, String(text));
          const diagnostics = await compileDiagnostics(
            wasmPath,
            uri,
            String(text),
          );
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics,
          });
        }
        return;
      }
      if (method === "textDocument/didChange") {
        const uri = msg.params?.textDocument?.uri;
        const changes = msg.params?.contentChanges;
        if (
          typeof uri === "string" && Array.isArray(changes) &&
          changes.length > 0
        ) {
          const text = String(changes[changes.length - 1].text ?? "");
          docs.set(uri, text);
          const diagnostics = await compileDiagnostics(wasmPath, uri, text);
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics,
          });
        }
        return;
      }
      if (method === "textDocument/didSave") {
        const uri = msg.params?.textDocument?.uri;
        if (typeof uri === "string") {
          const text = docs.get(uri) ?? "";
          const diagnostics = await compileDiagnostics(wasmPath, uri, text);
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics,
          });
        }
        return;
      }
      if (method === "textDocument/didClose") {
        const uri = msg.params?.textDocument?.uri;
        if (typeof uri === "string") {
          docs.delete(uri);
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics: [],
          });
        }
        return;
      }

      if (method === "textDocument/formatting") {
        const uri = msg.params?.textDocument?.uri;
        const text = typeof uri === "string" ? (docs.get(uri) ?? "") : "";
        const formatted = await formatSource(
          wasmPath,
          uri ?? "<unknown>",
          text,
        );
        await sendResponse(id, [{
          range: fullRangeForText(text),
          newText: formatted,
        }]);
        return;
      }

      if (id !== undefined) {
        await sendResponse(id, null);
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      if (id !== undefined) {
        await sendError(id, -32603, message);
      }
    }
  });
}

await main().catch(failWithError);
