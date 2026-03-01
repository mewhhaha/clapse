import { formatWasmOpcodes } from "../static/wasm_disasm.js";

const EMPTY_MODULE = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
]);

const SIMPLE_FUNCTION_MODULE = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
  // type section: one () -> ()
  0x01, 0x04, 0x01, 0x60, 0x00, 0x00,
  // function section: one function using type 0
  0x03, 0x02, 0x01, 0x00,
  // code section: one body with i32.const 7, drop, end
  0x0a, 0x07, 0x01, 0x05, 0x00, 0x41, 0x07, 0x1a, 0x0b,
]);

Deno.test("formatWasmOpcodes reports no code section for empty module", () => {
  const text = formatWasmOpcodes(EMPTY_MODULE);
  if (!text.includes("no code section")) {
    throw new Error(`Unexpected decode output: ${text}`);
  }
});

Deno.test("formatWasmOpcodes prints mnemonics for simple function", () => {
  const text = formatWasmOpcodes(SIMPLE_FUNCTION_MODULE);
  if (!text.includes("func[0]")) {
    throw new Error(`Missing function marker:\n${text}`);
  }
  if (!text.includes("i32.const 7")) {
    throw new Error(`Missing i32.const opcode:\n${text}`);
  }
  if (!text.includes("drop")) {
    throw new Error(`Missing drop opcode:\n${text}`);
  }
  if (!text.includes("end")) {
    throw new Error(`Missing end opcode:\n${text}`);
  }
});
