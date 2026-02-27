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
  if (err instanceof Error) {
    const debugStackRaw = isDeno
      ? String(Deno.env.get("CLAPSE_DEBUG_STACK") ?? "").toLowerCase()
      : "";
    const debugStackEnabled = debugStackRaw === "1" || debugStackRaw === "true" || debugStackRaw === "yes";
    if (debugStackEnabled && typeof err.stack === "string" && err.stack.length > 0) {
      console.error(err.stack);
      if (isDeno) {
        Deno.exit(1);
      }
      process.exit(1);
    }
    fail(err.message);
    return;
  }
  fail(String(err));
}
