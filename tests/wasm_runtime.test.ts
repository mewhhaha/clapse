import { extractWasmInstance } from "../static/wasm_runtime.js";

const EMPTY_WASM_MODULE = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
]);

Deno.test(
  "extractWasmInstance accepts instantiate(module, imports)",
  async () => {
    const module = await WebAssembly.compile(EMPTY_WASM_MODULE);
    const result = await WebAssembly.instantiate(module, {});
    const instance = extractWasmInstance(result);
    if (!(instance instanceof WebAssembly.Instance)) {
      throw new Error("Expected a WebAssembly.Instance.");
    }
  },
);

Deno.test(
  "extractWasmInstance accepts instantiate(bytes, imports)",
  async () => {
    const result = await WebAssembly.instantiate(EMPTY_WASM_MODULE, {});
    const instance = extractWasmInstance(result);
    if (!(instance instanceof WebAssembly.Instance)) {
      throw new Error("Expected a WebAssembly.Instance.");
    }
  },
);

Deno.test("extractWasmInstance throws for invalid values", () => {
  let didThrow = false;
  try {
    extractWasmInstance(null);
  } catch (err) {
    didThrow = true;
    const message = err instanceof Error ? err.message : String(err);
    if (message !== "WebAssembly.instantiate returned no instance.") {
      throw new Error(`Unexpected error message: ${message}`);
    }
  }
  if (!didThrow) {
    throw new Error("Expected extractWasmInstance to throw.");
  }
});
