export const isDeno = typeof Deno !== "undefined";

export function cliArgs() {
  if (isDeno) {
    return Deno.args;
  }
  return process.argv.slice(2);
}

export async function readBinaryFile(path) {
  if (isDeno) {
    return await Deno.readFile(path);
  }
  const fs = await import("node:fs/promises");
  return await fs.readFile(path);
}

export function nowNs() {
  if (typeof process !== "undefined" && process.hrtime?.bigint) {
    return process.hrtime.bigint();
  }
  if (typeof performance !== "undefined" && typeof performance.now === "function") {
    return BigInt(Math.round(performance.now() * 1_000_000));
  }
  return BigInt(Date.now() * 1_000_000);
}

export function fail(message) {
  console.error(message);
  if (isDeno) {
    Deno.exit(1);
  }
  process.exit(1);
}

export function failWithError(err) {
  const message = err instanceof Error ? err.message : String(err);
  fail(message);
}
