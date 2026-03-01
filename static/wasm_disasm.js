const MAGIC = [0x00, 0x61, 0x73, 0x6d];
const VERSION = [0x01, 0x00, 0x00, 0x00];

const BLOCKTYPE_NAMES = {
  0x40: "void",
  0x7f: "i32",
  0x7e: "i64",
  0x7d: "f32",
  0x7c: "f64",
  0x7b: "v128",
  0x70: "funcref",
  0x6f: "externref",
};

const VALTYPE_NAMES = {
  0x7f: "i32",
  0x7e: "i64",
  0x7d: "f32",
  0x7c: "f64",
  0x7b: "v128",
  0x70: "funcref",
  0x6f: "externref",
};

const OPCODE_NAMES = {
  0x00: "unreachable",
  0x01: "nop",
  0x02: "block",
  0x03: "loop",
  0x04: "if",
  0x05: "else",
  0x0b: "end",
  0x0c: "br",
  0x0d: "br_if",
  0x0e: "br_table",
  0x0f: "return",
  0x10: "call",
  0x11: "call_indirect",
  0x12: "return_call",
  0x13: "return_call_indirect",
  0x14: "call_ref",
  0x15: "return_call_ref",
  0x1a: "drop",
  0x1b: "select",
  0x1c: "select_t",
  0x20: "local.get",
  0x21: "local.set",
  0x22: "local.tee",
  0x23: "global.get",
  0x24: "global.set",
  0x25: "table.get",
  0x26: "table.set",
  0x28: "i32.load",
  0x29: "i64.load",
  0x2a: "f32.load",
  0x2b: "f64.load",
  0x2c: "i32.load8_s",
  0x2d: "i32.load8_u",
  0x2e: "i32.load16_s",
  0x2f: "i32.load16_u",
  0x30: "i64.load8_s",
  0x31: "i64.load8_u",
  0x32: "i64.load16_s",
  0x33: "i64.load16_u",
  0x34: "i64.load32_s",
  0x35: "i64.load32_u",
  0x36: "i32.store",
  0x37: "i64.store",
  0x38: "f32.store",
  0x39: "f64.store",
  0x3a: "i32.store8",
  0x3b: "i32.store16",
  0x3c: "i64.store8",
  0x3d: "i64.store16",
  0x3e: "i64.store32",
  0x3f: "memory.size",
  0x40: "memory.grow",
  0x41: "i32.const",
  0x42: "i64.const",
  0x43: "f32.const",
  0x44: "f64.const",
  0x45: "i32.eqz",
  0x46: "i32.eq",
  0x47: "i32.ne",
  0x48: "i32.lt_s",
  0x49: "i32.lt_u",
  0x4a: "i32.gt_s",
  0x4b: "i32.gt_u",
  0x4c: "i32.le_s",
  0x4d: "i32.le_u",
  0x4e: "i32.ge_s",
  0x4f: "i32.ge_u",
  0x50: "i64.eqz",
  0x51: "i64.eq",
  0x52: "i64.ne",
  0x53: "i64.lt_s",
  0x54: "i64.lt_u",
  0x55: "i64.gt_s",
  0x56: "i64.gt_u",
  0x57: "i64.le_s",
  0x58: "i64.le_u",
  0x59: "i64.ge_s",
  0x5a: "i64.ge_u",
  0x5b: "f32.eq",
  0x5c: "f32.ne",
  0x5d: "f32.lt",
  0x5e: "f32.gt",
  0x5f: "f32.le",
  0x60: "f32.ge",
  0x61: "f64.eq",
  0x62: "f64.ne",
  0x63: "f64.lt",
  0x64: "f64.gt",
  0x65: "f64.le",
  0x66: "f64.ge",
  0x67: "i32.clz",
  0x68: "i32.ctz",
  0x69: "i32.popcnt",
  0x6a: "i32.add",
  0x6b: "i32.sub",
  0x6c: "i32.mul",
  0x6d: "i32.div_s",
  0x6e: "i32.div_u",
  0x6f: "i32.rem_s",
  0x70: "i32.rem_u",
  0x71: "i32.and",
  0x72: "i32.or",
  0x73: "i32.xor",
  0x74: "i32.shl",
  0x75: "i32.shr_s",
  0x76: "i32.shr_u",
  0x77: "i32.rotl",
  0x78: "i32.rotr",
  0x79: "i64.clz",
  0x7a: "i64.ctz",
  0x7b: "i64.popcnt",
  0x7c: "i64.add",
  0x7d: "i64.sub",
  0x7e: "i64.mul",
  0x7f: "i64.div_s",
  0x80: "i64.div_u",
  0x81: "i64.rem_s",
  0x82: "i64.rem_u",
  0x83: "i64.and",
  0x84: "i64.or",
  0x85: "i64.xor",
  0x86: "i64.shl",
  0x87: "i64.shr_s",
  0x88: "i64.shr_u",
  0x89: "i64.rotl",
  0x8a: "i64.rotr",
  0x8b: "f32.abs",
  0x8c: "f32.neg",
  0x8d: "f32.ceil",
  0x8e: "f32.floor",
  0x8f: "f32.trunc",
  0x90: "f32.nearest",
  0x91: "f32.sqrt",
  0x92: "f32.add",
  0x93: "f32.sub",
  0x94: "f32.mul",
  0x95: "f32.div",
  0x96: "f32.min",
  0x97: "f32.max",
  0x98: "f32.copysign",
  0x99: "f64.abs",
  0x9a: "f64.neg",
  0x9b: "f64.ceil",
  0x9c: "f64.floor",
  0x9d: "f64.trunc",
  0x9e: "f64.nearest",
  0x9f: "f64.sqrt",
  0xa0: "f64.add",
  0xa1: "f64.sub",
  0xa2: "f64.mul",
  0xa3: "f64.div",
  0xa4: "f64.min",
  0xa5: "f64.max",
  0xa6: "f64.copysign",
  0xa7: "i32.wrap_i64",
  0xa8: "i32.trunc_f32_s",
  0xa9: "i32.trunc_f32_u",
  0xaa: "i32.trunc_f64_s",
  0xab: "i32.trunc_f64_u",
  0xac: "i64.extend_i32_s",
  0xad: "i64.extend_i32_u",
  0xae: "i64.trunc_f32_s",
  0xaf: "i64.trunc_f32_u",
  0xb0: "i64.trunc_f64_s",
  0xb1: "i64.trunc_f64_u",
  0xb2: "f32.convert_i32_s",
  0xb3: "f32.convert_i32_u",
  0xb4: "f32.convert_i64_s",
  0xb5: "f32.convert_i64_u",
  0xb6: "f32.demote_f64",
  0xb7: "f64.convert_i32_s",
  0xb8: "f64.convert_i32_u",
  0xb9: "f64.convert_i64_s",
  0xba: "f64.convert_i64_u",
  0xbb: "f64.promote_f32",
  0xbc: "i32.reinterpret_f32",
  0xbd: "i64.reinterpret_f64",
  0xbe: "f32.reinterpret_i32",
  0xbf: "f64.reinterpret_i64",
  0xc0: "i32.extend8_s",
  0xc1: "i32.extend16_s",
  0xc2: "i64.extend8_s",
  0xc3: "i64.extend16_s",
  0xc4: "i64.extend32_s",
  0xd0: "ref.null",
  0xd1: "ref.is_null",
  0xd2: "ref.func",
  0xfc: "misc",
  0xfd: "simd",
};

const MEMARG_OPS = new Set([
  0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34,
  0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e,
]);

const FC_NAMES = {
  0: "i32.trunc_sat_f32_s",
  1: "i32.trunc_sat_f32_u",
  2: "i32.trunc_sat_f64_s",
  3: "i32.trunc_sat_f64_u",
  4: "i64.trunc_sat_f32_s",
  5: "i64.trunc_sat_f32_u",
  6: "i64.trunc_sat_f64_s",
  7: "i64.trunc_sat_f64_u",
  8: "memory.init",
  9: "data.drop",
  10: "memory.copy",
  11: "memory.fill",
  12: "table.init",
  13: "elem.drop",
  14: "table.copy",
  15: "table.grow",
  16: "table.size",
  17: "table.fill",
};

class Reader {
  constructor(bytes, start = 0, end = bytes.length) {
    this.bytes = bytes;
    this.offset = start;
    this.end = end;
  }

  eof() {
    return this.offset >= this.end;
  }

  ensure(size) {
    if (this.offset + size > this.end) {
      throw new Error(
        `unexpected EOF at 0x${this.offset.toString(16)} (need ${size} byte(s))`,
      );
    }
  }

  peekByte() {
    this.ensure(1);
    return this.bytes[this.offset];
  }

  readByte() {
    this.ensure(1);
    return this.bytes[this.offset++];
  }

  readBytes(size) {
    this.ensure(size);
    const out = this.bytes.subarray(this.offset, this.offset + size);
    this.offset += size;
    return out;
  }

  readVarU32() {
    let result = 0;
    let shift = 0;
    for (let i = 0; i < 5; i += 1) {
      const byte = this.readByte();
      result += (byte & 0x7f) * 2 ** shift;
      if ((byte & 0x80) === 0) {
        return result >>> 0;
      }
      shift += 7;
    }
    throw new Error(`invalid varuint32 at 0x${this.offset.toString(16)}`);
  }

  readVarS32() {
    let result = 0;
    let shift = 0;
    let byte = 0;
    for (let i = 0; i < 5; i += 1) {
      byte = this.readByte();
      result |= (byte & 0x7f) << shift;
      shift += 7;
      if ((byte & 0x80) === 0) {
        if (shift < 32 && (byte & 0x40) !== 0) {
          result |= ~0 << shift;
        }
        return result | 0;
      }
    }
    throw new Error(`invalid varint32 at 0x${this.offset.toString(16)}`);
  }

  readVarS64() {
    let result = 0n;
    let shift = 0n;
    let byte = 0n;
    for (let i = 0; i < 10; i += 1) {
      byte = BigInt(this.readByte());
      result |= (byte & 0x7fn) << shift;
      shift += 7n;
      if ((byte & 0x80n) === 0n) {
        if (shift < 64n && (byte & 0x40n) !== 0n) {
          result |= ~0n << shift;
        }
        return result;
      }
    }
    throw new Error(`invalid varint64 at 0x${this.offset.toString(16)}`);
  }
}

function toHexOffset(value) {
  return value.toString(16).padStart(8, "0");
}

function opcodeName(opcode) {
  return (
    OPCODE_NAMES[opcode] ?? `opcode_0x${opcode.toString(16).padStart(2, "0")}`
  );
}

function readValType(reader) {
  const byte = reader.readByte();
  return VALTYPE_NAMES[byte] ?? `type_0x${byte.toString(16).padStart(2, "0")}`;
}

function readBlockType(reader) {
  const first = reader.peekByte();
  if (Object.hasOwn(BLOCKTYPE_NAMES, first)) {
    reader.readByte();
    return BLOCKTYPE_NAMES[first];
  }
  return `typeidx ${reader.readVarS32()}`;
}

function readMemArg(reader) {
  const align = reader.readVarU32();
  const offset = reader.readVarU32();
  return `align=${align} offset=${offset}`;
}

function pushLine(lines, offset, indent, text) {
  lines.push(`${toHexOffset(offset)}  ${"  ".repeat(indent)}${text}`);
}

function parseFcInstruction(reader) {
  const sub = reader.readVarU32();
  const name = FC_NAMES[sub] ?? `misc.${sub}`;
  if (sub <= 7 || sub === 13) {
    return name;
  }
  if (sub === 8) {
    const dataIndex = reader.readVarU32();
    const memoryIndex = reader.readVarU32();
    return `${name} data=${dataIndex} memory=${memoryIndex}`;
  }
  if (sub === 9) {
    const dataIndex = reader.readVarU32();
    return `${name} data=${dataIndex}`;
  }
  if (sub === 10) {
    const dst = reader.readVarU32();
    const src = reader.readVarU32();
    return `${name} dst_memory=${dst} src_memory=${src}`;
  }
  if (sub === 11) {
    const memoryIndex = reader.readVarU32();
    return `${name} memory=${memoryIndex}`;
  }
  if (sub === 12) {
    const elem = reader.readVarU32();
    const table = reader.readVarU32();
    return `${name} elem=${elem} table=${table}`;
  }
  if (sub === 14) {
    const dst = reader.readVarU32();
    const src = reader.readVarU32();
    return `${name} dst_table=${dst} src_table=${src}`;
  }
  if (sub === 15 || sub === 16 || sub === 17) {
    const table = reader.readVarU32();
    return `${name} table=${table}`;
  }
  return `${name} (unparsed immediate)`;
}

function parseInstructionImmediate(reader, opcode) {
  if (
    opcode === 0x0c ||
    opcode === 0x0d ||
    opcode === 0x10 ||
    opcode === 0x12
  ) {
    return String(reader.readVarU32());
  }
  if (
    opcode === 0x20 ||
    opcode === 0x21 ||
    opcode === 0x22 ||
    opcode === 0x23 ||
    opcode === 0x24 ||
    opcode === 0x25 ||
    opcode === 0x26 ||
    opcode === 0xd2
  ) {
    return String(reader.readVarU32());
  }
  if (opcode === 0x11 || opcode === 0x13) {
    const typeIndex = reader.readVarU32();
    const tableIndex = reader.readVarU32();
    return `type=${typeIndex} table=${tableIndex}`;
  }
  if (opcode === 0x14 || opcode === 0x15) {
    return `type=${reader.readVarU32()}`;
  }
  if (opcode === 0x0e) {
    const count = reader.readVarU32();
    const labels = [];
    for (let i = 0; i < count; i += 1) {
      labels.push(reader.readVarU32());
    }
    const fallback = reader.readVarU32();
    return `[${labels.join(", ")}] default=${fallback}`;
  }
  if (MEMARG_OPS.has(opcode)) {
    return readMemArg(reader);
  }
  if (opcode === 0x3f || opcode === 0x40) {
    return `memory=${reader.readVarU32()}`;
  }
  if (opcode === 0x41) {
    return String(reader.readVarS32());
  }
  if (opcode === 0x42) {
    return reader.readVarS64().toString();
  }
  if (opcode === 0x43) {
    const bytes = reader.readBytes(4);
    const value = new DataView(
      bytes.buffer,
      bytes.byteOffset,
      bytes.byteLength,
    ).getFloat32(0, true);
    return String(value);
  }
  if (opcode === 0x44) {
    const bytes = reader.readBytes(8);
    const value = new DataView(
      bytes.buffer,
      bytes.byteOffset,
      bytes.byteLength,
    ).getFloat64(0, true);
    return String(value);
  }
  if (opcode === 0x1c) {
    const count = reader.readVarU32();
    const types = [];
    for (let i = 0; i < count; i += 1) {
      types.push(readValType(reader));
    }
    return types.join(", ");
  }
  if (opcode === 0xd0) {
    return readValType(reader);
  }
  if (opcode === 0xfc) {
    return parseFcInstruction(reader);
  }
  if (opcode === 0xfd) {
    const sub = reader.readVarU32();
    return `simd.${sub} (immediates not decoded)`;
  }
  return "";
}

function decodeExpression(reader, endOffset, lines, state, indent) {
  while (reader.offset < endOffset) {
    if (state.instructions >= state.maxInstructions) {
      state.truncated = true;
      return "truncated";
    }
    const offset = reader.offset;
    const opcode = reader.readByte();
    state.instructions += 1;

    if (opcode === 0x0b) {
      pushLine(lines, offset, indent, "end");
      return "end";
    }
    if (opcode === 0x05) {
      pushLine(lines, offset, indent, "else");
      return "else";
    }

    if (opcode === 0x02 || opcode === 0x03) {
      const type = readBlockType(reader);
      pushLine(lines, offset, indent, `${opcodeName(opcode)} ${type}`);
      const nested = decodeExpression(
        reader,
        endOffset,
        lines,
        state,
        indent + 1,
      );
      if (nested !== "end" && nested !== "truncated") {
        pushLine(lines, reader.offset, indent + 1, "(unterminated block)");
      }
      if (nested === "truncated") return "truncated";
      continue;
    }

    if (opcode === 0x04) {
      const type = readBlockType(reader);
      pushLine(lines, offset, indent, `${opcodeName(opcode)} ${type}`);
      const thenEnd = decodeExpression(
        reader,
        endOffset,
        lines,
        state,
        indent + 1,
      );
      if (thenEnd === "truncated") return "truncated";
      if (thenEnd === "else") {
        const elseEnd = decodeExpression(
          reader,
          endOffset,
          lines,
          state,
          indent + 1,
        );
        if (elseEnd === "truncated") return "truncated";
      }
      continue;
    }

    const imm = parseInstructionImmediate(reader, opcode);
    pushLine(
      lines,
      offset,
      indent,
      imm.length > 0 ? `${opcodeName(opcode)} ${imm}` : opcodeName(opcode),
    );
  }
  return "eof";
}

function decodeCodeSection(reader, sectionEnd, state) {
  const lines = [];
  const functionCount = reader.readVarU32();
  lines.push(`; code section functions: ${functionCount}`);

  for (let fn = 0; fn < functionCount; fn += 1) {
    if (reader.offset >= sectionEnd) break;
    const bodySize = reader.readVarU32();
    const bodyStart = reader.offset;
    const bodyEnd = bodyStart + bodySize;
    if (bodyEnd > sectionEnd) {
      throw new Error(`function body overflow at func[${fn}]`);
    }

    lines.push(``);
    lines.push(`func[${fn}] size=${bodySize}`);

    const localDeclCount = reader.readVarU32();
    const locals = [];
    for (let i = 0; i < localDeclCount; i += 1) {
      const count = reader.readVarU32();
      const type = readValType(reader);
      locals.push(`${count} x ${type}`);
    }
    lines.push(`locals: ${locals.length > 0 ? locals.join(", ") : "(none)"}`);

    const terminator = decodeExpression(reader, bodyEnd, lines, state, 1);
    if (terminator !== "end" && terminator !== "truncated") {
      lines.push(`(function missing end opcode)`);
    }

    if (state.truncated) {
      lines.push(
        `... opcode output truncated at ${state.maxInstructions} instruction(s)`,
      );
      reader.offset = bodyEnd;
      break;
    }
    reader.offset = bodyEnd;
  }

  if (reader.offset < sectionEnd) {
    lines.push(``);
    lines.push(`; skipped ${sectionEnd - reader.offset} trailing byte(s)`);
    reader.offset = sectionEnd;
  }

  return lines.join("\n").trim();
}

export function formatWasmOpcodes(bytes, options = {}) {
  try {
    if (!(bytes instanceof Uint8Array)) {
      throw new Error("expected Uint8Array input");
    }

    const reader = new Reader(bytes);
    const magic = reader.readBytes(4);
    const version = reader.readBytes(4);
    for (let i = 0; i < 4; i += 1) {
      if (magic[i] !== MAGIC[i]) throw new Error("invalid wasm magic header");
      if (version[i] !== VERSION[i])
        throw new Error("unsupported wasm version");
    }

    const state = {
      instructions: 0,
      maxInstructions:
        Number(options.maxInstructions) > 0
          ? Number(options.maxInstructions)
          : 20000,
      truncated: false,
    };

    while (!reader.eof()) {
      const sectionId = reader.readByte();
      const sectionSize = reader.readVarU32();
      const sectionStart = reader.offset;
      const sectionEnd = sectionStart + sectionSize;
      if (sectionEnd > bytes.length) {
        throw new Error(`section ${sectionId} exceeds module length`);
      }

      if (sectionId === 10) {
        return decodeCodeSection(reader, sectionEnd, state);
      }
      reader.offset = sectionEnd;
    }

    return "(no code section found)";
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return `Opcode decode failed: ${message}`;
  }
}
