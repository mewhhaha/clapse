export function extractWasmInstance(result) {
  if (result instanceof WebAssembly.Instance) {
    return result;
  }
  if (
    result &&
    typeof result === "object" &&
    "instance" in result &&
    result.instance instanceof WebAssembly.Instance
  ) {
    return result.instance;
  }
  throw new Error("WebAssembly.instantiate returned no instance.");
}
