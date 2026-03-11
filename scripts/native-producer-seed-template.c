#include <stdint.h>
#include <stddef.h>

#define PAGE_SIZE 65536u

typedef struct {
  uint32_t ptr;
  int32_t len;
} SliceDesc;

typedef struct {
  uint32_t ptr;
  uint32_t len;
  int ok;
} Segment;

extern unsigned char __heap_base;

static uint32_t heap_ptr = 0;

static const char SEED_WASM_BASE64[] =
  {{SEED_WASM_BASE64_LITERAL}}
;
static const uint32_t SEED_WASM_BASE64_LEN = (uint32_t) (sizeof(SEED_WASM_BASE64) - 1u);
static const char MINI_WASM_BASE64[] =
  "AGFzbQEAAAABBgFgAX8BfwMDAgAABQMBAAIGCAF/AUGAiAQLBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAIAALAE4EbmFtZQAeHWNsYXBzZV9taW5fY29tcGlsZXJfc3R1Yi53YXNtARMCAApjbGFwc2VfcnVuAQRtYWluBxIBAA9fX3N0YWNrX3BvaW50ZXIAJglwcm9kdWNlcnMBDHByb2Nlc3NlZC1ieQEFY2xhbmcGMjEuMS44AJQBD3RhcmdldF9mZWF0dXJlcwgrC2J1bGstbWVtb3J5Kw9idWxrLW1lbW9yeS1vcHQrFmNhbGwtaW5kaXJlY3Qtb3ZlcmxvbmcrCm11bHRpdmFsdWUrD211dGFibGUtZ2xvYmFscysTbm9udHJhcHBpbmctZnB0b2ludCsPcmVmZXJlbmNlLXR5cGVzKwhzaWduLWV4dA==";

static const char SOURCE_VERSION[] = "{{SOURCE_VERSION_LITERAL}}";

static const char JSON_ERROR_PREFIX[] = "{\"ok\":false,\"error\":\"";
static const char JSON_ERROR_SUFFIX[] = "\"}";
static const char ENTRYPOINT_ROOT_INVALID_ERROR[] =
  "compile request entrypoint_exports contains invalid root";
static const char ENTRYPOINT_ROOT_UNKNOWN_ERROR[] = "unknown entrypoint root";
static const char BASE64_ALPHABET[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static const char COMPILE_PREFIX[] = "{\"ok\":true,\"backend\":\"kernel-native\",\"wasm_base64\":\"";
static const char COMPILE_MID_A[] = "\",\"public_exports\":[{\"name\":\"main\",\"arity\":1}],\"abi_exports\":[{\"name\":\"clapse_run\",\"arity\":1}],\"dts\":\"export declare function clapse_run(request_handle: number): number;\\nexport declare function main(arg0: number): number;\\n\",\"artifacts\":{\"lowered_ir.txt\":\"(lowered_ir)\\nphase: kernel-native-phase1\\nkind: normalized-source\\n";
static const char COMPILE_MID_B[] = "\",\"collapsed_ir.txt\":\"(collapsed_ir)\\nphase: kernel-native-phase1\\nkind: normalized-source\\n";
static const char COMPILE_SUFFIX_A[] = "\"},\"__clapse_contract\":{\"source_version\":\"";
static const char COMPILE_SUFFIX_B[] = "\",\"compile_contract_version\":\"native-v1\"}}";
static const char COMPILE_DYNAMIC_MID_A[] = "\",\"public_exports\":";
static const char COMPILE_DYNAMIC_MID_B[] = ",\"abi_exports\":[],\"dts\":\"";
static const char COMPILE_DYNAMIC_MID_C[] = "\",\"artifacts\":{\"lowered_ir.txt\":\"(lowered_ir)\\nphase: kernel-native-phase1\\nkind: normalized-source\\n";

static const char SELFHOST_PREFIX[] = "{\"ok\":true,\"backend\":\"kernel-native\",\"wasm_base64\":\"";
static const char SELFHOST_MID_A[] = "\",\"artifacts\":{\"lowered_ir.txt\":\"(lowered_ir)\\nphase: kernel-native-phase1\\nkind: normalized-source\\n";
static const char SELFHOST_MID_B[] = "\",\"collapsed_ir.txt\":\"(collapsed_ir)\\nphase: kernel-native-phase1\\nkind: normalized-source\\n";
static const char SELFHOST_SUFFIX[] = "\"}}";
static const char TAIL_SELF_PREFIX[] = "\\n-- VSelfTailCall ";
static const char TAIL_MUTUAL_PREFIX[] = "\\n-- VMutualTailCall ";
static const char TAIL_ARROW[] = " -> ";

static const char FORMAT_PREFIX[] = "{\"ok\":true,\"formatted\":\"";
static const char FORMAT_SUFFIX[] = "\"}";

static const char PARSE_PREFIX[] = "{\"ok\":true,\"artifacts\":{\"parsed_cst.txt\":\"";
static const char PARSE_SUFFIX[] = "\"}}";

static const char EMIT_WAT_PREFIX[] = "{\"ok\":true,\"wat\":\"";
static const char EMIT_WAT_SUFFIX[] = "\"}";
static const char EMIT_WAT_TEMPLATE[] =
  "{\"ok\":true,\"wat\":\"(module\\n  (memory (export \\\"__memory\\\") 1)\\n)\"}";

#define MAX_FN_DECLS 1024u
#define MAX_ROOTS 512u
#define MAX_TEMP_BINDINGS 512u
#define MAX_TEMP_LINES 512u

typedef struct {
  uint32_t ptr;
  uint32_t len;
  int ok;
} NameSpan;

typedef struct {
  int ok;
  int32_t value;
} EvalConst;

#define EVAL_VALUE_INT 1u
#define EVAL_VALUE_SLICE 2u
#define MAX_EVAL_LOCALS 32u
#define MAX_EVAL_ARGS 8u
#define MAX_EVAL_SLICE_BYTES 256u

typedef struct {
  int ok;
  uint32_t kind;
  int32_t int_value;
  uint32_t slice_len;
  uint8_t slice_bytes[MAX_EVAL_SLICE_BYTES];
} EvalValue;

typedef struct {
  NameSpan name;
  uint32_t line_start;
  uint32_t line_end;
  uint32_t body_start;
  uint32_t body_end;
} FnDecl;

static uint32_t collect_fn_decls(Segment source, FnDecl *decls, uint32_t max_decls);
static uint32_t find_case_of_at(Segment source, uint32_t start, uint32_t end);
static uint32_t find_case_arm_arrow(Segment source, uint32_t start, uint32_t end);
static uint32_t find_top_level_assignment_eq(Segment source, uint32_t start, uint32_t end);
static uint32_t find_top_level_clause_bar(Segment source, uint32_t start, uint32_t end);
static int namespan_starts_with_upper(Segment source, NameSpan name);

typedef struct {
  uint32_t line_start;
  uint32_t line_end;
  uint32_t content_end;
  int is_temp_binding;
  int keep_line;
  uint32_t old_temp;
  uint32_t rhs_start;
  uint32_t rhs_end;
} TempLine;

/*
 * Keep large scratch buffers out of the wasm stack to avoid clobbering
 * embedded static data on large artifacts.
 */
static TempLine temp_lines_workspace[MAX_TEMP_LINES];
static uint32_t temp_slots_workspace[MAX_TEMP_BINDINGS];
static int temp_live_workspace[MAX_TEMP_BINDINGS];
static uint32_t renumber_slots_workspace[MAX_TEMP_BINDINGS];
static NameSpan roots_workspace[MAX_ROOTS];
static FnDecl fn_decls_workspace[MAX_FN_DECLS];
static int reachable_workspace[MAX_FN_DECLS];
static int tail_target_workspace[MAX_FN_DECLS];
static int self_tail_workspace[MAX_FN_DECLS];
static int mutual_tail_workspace[MAX_FN_DECLS];

static uint32_t cstr_len(const char *s) {
  uint32_t len = 0;
  while (s[len] != '\0') {
    len += 1;
  }
  return len;
}

static uint32_t align_up(uint32_t value, uint32_t align) {
  uint32_t mask = align - 1u;
  return (value + mask) & (~mask);
}

static int ensure_capacity(uint32_t end) {
  uint32_t current_bytes = __builtin_wasm_memory_size(0) * PAGE_SIZE;
  if (end <= current_bytes) {
    return 1;
  }
  uint32_t missing = end - current_bytes;
  uint32_t pages = (missing + PAGE_SIZE - 1u) / PAGE_SIZE;
  uint32_t grown = __builtin_wasm_memory_grow(0, pages);
  return grown != 0xffffffffu;
}

static void init_heap_ptr(void) {
  if (heap_ptr != 0u) {
    return;
  }
  uintptr_t base = (uintptr_t) &__heap_base;
  if (base < 1024u) {
    base = 1024u;
  }
  heap_ptr = (uint32_t) base;
}

static uint32_t alloc_bytes(uint32_t len, uint32_t align) {
  init_heap_ptr();
  uint32_t start = align_up(heap_ptr, align);
  if (start > 0xffffffffu - len) {
    return 0u;
  }
  uint32_t end = start + len;
  if (!ensure_capacity(end)) {
    return 0u;
  }
  heap_ptr = end;
  return start;
}

static int is_ws(uint8_t b) {
  return b == ' ' || b == '\n' || b == '\r' || b == '\t';
}

static uint32_t find_bytes(uint32_t hay_ptr, uint32_t hay_len, const char *needle,
                           uint32_t needle_len, uint32_t start) {
  if (needle_len == 0u) {
    return start;
  }
  if (hay_len < needle_len || start > hay_len - needle_len) {
    return hay_len;
  }
  uint8_t *hay = (uint8_t *) (uintptr_t) hay_ptr;
  for (uint32_t i = start; i + needle_len <= hay_len; i += 1) {
    uint32_t j = 0;
    while (j < needle_len && hay[i + j] == (uint8_t) needle[j]) {
      j += 1;
    }
    if (j == needle_len) {
      return i;
    }
  }
  return hay_len;
}

static Segment missing_segment(void) {
  Segment out;
  out.ptr = 0u;
  out.len = 0u;
  out.ok = 0;
  return out;
}

static Segment find_json_string_segment(uint32_t req_ptr, uint32_t req_len, const char *key) {
  uint32_t key_len = cstr_len(key);
  uint32_t at = find_bytes(req_ptr, req_len, key, key_len, 0u);
  if (at == req_len) {
    return missing_segment();
  }
  uint8_t *req = (uint8_t *) (uintptr_t) req_ptr;
  uint32_t i = at + key_len;
  while (i < req_len && req[i] != ':') {
    i += 1;
  }
  if (i >= req_len) {
    return missing_segment();
  }
  i += 1;
  while (i < req_len && is_ws(req[i])) {
    i += 1;
  }
  if (i >= req_len || req[i] != '"') {
    return missing_segment();
  }
  i += 1;
  uint32_t start = i;
  int escaped = 0;
  while (i < req_len) {
    uint8_t c = req[i];
    if (escaped) {
      escaped = 0;
      i += 1;
      continue;
    }
    if (c == '\\') {
      escaped = 1;
      i += 1;
      continue;
    }
    if (c == '"') {
      Segment out;
      out.ptr = req_ptr + start;
      out.len = i - start;
      out.ok = 1;
      return out;
    }
    i += 1;
  }
  return missing_segment();
}

static Segment find_source_segment(uint32_t req_ptr, uint32_t req_len) {
  Segment source = find_json_string_segment(req_ptr, req_len, "\"input_source\"");
  if (source.ok && source.len > 0u) {
    return source;
  }
  source = find_json_string_segment(req_ptr, req_len, "\"source\"");
  if (source.ok && source.len > 0u) {
    return source;
  }
  return missing_segment();
}

static Segment decode_json_source_segment(Segment source) {
  if (!source.ok) {
    return source;
  }
  uint32_t out_ptr = alloc_bytes(source.len + 1u, 1u);
  if (out_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *src = (uint8_t *) (uintptr_t) source.ptr;
  uint8_t *dst = (uint8_t *) (uintptr_t) out_ptr;
  uint32_t out_len = 0u;
  for (uint32_t at = 0u; at < source.len; at += 1u) {
    if (src[at] == '\\' && at + 1u < source.len) {
      uint8_t next = src[at + 1u];
      if (next == 'n') {
        dst[out_len++] = '\n';
        at += 1u;
        continue;
      }
      if (next == 'r') {
        dst[out_len++] = '\r';
        at += 1u;
        continue;
      }
      if (next == 't') {
        dst[out_len++] = '\t';
        at += 1u;
        continue;
      }
      if (next == '"' || next == '\\') {
        dst[out_len++] = next;
        at += 1u;
        continue;
      }
    }
    dst[out_len++] = src[at];
  }
  Segment out;
  out.ptr = out_ptr;
  out.len = out_len;
  out.ok = 1;
  return out;
}

static Segment clone_segment(Segment seg);

static Segment clone_decoded_source_segment(Segment source) {
  Segment cloned = clone_segment(source);
  if (!cloned.ok || cloned.len == 0u) {
    return missing_segment();
  }
  Segment decoded = decode_json_source_segment(cloned);
  if (!decoded.ok || decoded.len == 0u) {
    return cloned;
  }
  return decoded;
}

static int segment_equals_literal(Segment seg, const char *literal) {
  uint32_t lit_len = cstr_len(literal);
  if (!seg.ok || seg.len != lit_len) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) seg.ptr;
  for (uint32_t i = 0; i < lit_len; i += 1) {
    if (mem[i] != (uint8_t) literal[i]) {
      return 0;
    }
  }
  return 1;
}

static int segment_ends_with_literal(Segment seg, const char *literal) {
  uint32_t lit_len = cstr_len(literal);
  if (!seg.ok || seg.len < lit_len) {
    return 0;
  }
  uint8_t *data = (uint8_t *) (uintptr_t) (seg.ptr + seg.len - lit_len);
  for (uint32_t i = 0; i < lit_len; i += 1) {
    if (data[i] != (uint8_t) literal[i]) {
      return 0;
    }
  }
  return 1;
}

static int segment_is_kernel_compiler_input_path(Segment seg) {
  return segment_equals_literal(seg, "lib/compiler/kernel.clapse") ||
    segment_ends_with_literal(seg, "/lib/compiler/kernel.clapse");
}

static uint32_t make_slice_response(uint32_t payload_len, uint32_t *payload_out) {
  uint32_t desc_ptr = alloc_bytes(8u, 4u);
  if (desc_ptr == 0u) {
    return 0u;
  }
  uint32_t payload_ptr = alloc_bytes(payload_len, 1u);
  if (payload_ptr == 0u) {
    return 0u;
  }
  SliceDesc *desc = (SliceDesc *) (uintptr_t) desc_ptr;
  desc->ptr = payload_ptr;
  desc->len = (int32_t) payload_len;
  *payload_out = payload_ptr;
  return desc_ptr;
}

static void write_literal(uint8_t *dst, uint32_t *cursor, const char *literal) {
  uint32_t len = cstr_len(literal);
  for (uint32_t i = 0; i < len; i += 1) {
    dst[*cursor + i] = (uint8_t) literal[i];
  }
  *cursor += len;
}

static void write_segment(uint8_t *dst, uint32_t *cursor, Segment seg) {
  uint8_t *src = (uint8_t *) (uintptr_t) seg.ptr;
  for (uint32_t i = 0; i < seg.len; i += 1) {
    dst[*cursor + i] = src[i];
  }
  *cursor += seg.len;
}

static uint32_t segment_json_escaped_len(Segment seg) {
  if (!seg.ok || seg.len == 0u) {
    return 0u;
  }
  uint8_t *src = (uint8_t *) (uintptr_t) seg.ptr;
  uint32_t out = 0u;
  for (uint32_t i = 0u; i < seg.len; i += 1u) {
    uint8_t c = src[i];
    if (c == '"' || c == '\\' || c == '\n' || c == '\r' || c == '\t') {
      out += 2u;
    } else {
      out += 1u;
    }
  }
  return out;
}

static void write_json_escaped_segment(uint8_t *dst, uint32_t *cursor, Segment seg) {
  if (!seg.ok || seg.len == 0u) {
    return;
  }
  uint8_t *src = (uint8_t *) (uintptr_t) seg.ptr;
  for (uint32_t i = 0u; i < seg.len; i += 1u) {
    uint8_t c = src[i];
    if (c == '"' || c == '\\') {
      dst[*cursor] = '\\';
      dst[*cursor + 1u] = c;
      *cursor += 2u;
      continue;
    }
    if (c == '\n') {
      dst[*cursor] = '\\';
      dst[*cursor + 1u] = 'n';
      *cursor += 2u;
      continue;
    }
    if (c == '\r') {
      dst[*cursor] = '\\';
      dst[*cursor + 1u] = 'r';
      *cursor += 2u;
      continue;
    }
    if (c == '\t') {
      dst[*cursor] = '\\';
      dst[*cursor + 1u] = 't';
      *cursor += 2u;
      continue;
    }
    dst[*cursor] = c;
    *cursor += 1u;
  }
}

static void write_name_span(uint8_t *dst, uint32_t *cursor, NameSpan name) {
  if (!name.ok || name.len == 0u) {
    return;
  }
  uint8_t *src = (uint8_t *) (uintptr_t) name.ptr;
  for (uint32_t i = 0; i < name.len; i += 1) {
    dst[*cursor + i] = src[i];
  }
  *cursor += name.len;
}

static uint32_t name_span_json_escaped_len(NameSpan name) {
  if (!name.ok || name.len == 0u) {
    return 0u;
  }
  uint8_t *src = (uint8_t *) (uintptr_t) name.ptr;
  uint32_t out = 0u;
  for (uint32_t i = 0; i < name.len; i += 1u) {
    if (src[i] == '"' || src[i] == '\\') {
      out += 2u;
    } else {
      out += 1u;
    }
  }
  return out;
}

static void write_json_escaped_name_span(uint8_t *dst, uint32_t *cursor, NameSpan name) {
  if (!name.ok || name.len == 0u) {
    return;
  }
  uint8_t *src = (uint8_t *) (uintptr_t) name.ptr;
  for (uint32_t i = 0; i < name.len; i += 1u) {
    if (src[i] == '"' || src[i] == '\\') {
      dst[*cursor] = '\\';
      *cursor += 1u;
    }
    dst[*cursor] = src[i];
    *cursor += 1u;
  }
}

static uint32_t encode_var_u32_bytes(uint32_t value, uint8_t *out) {
  uint32_t cursor = 0u;
  uint32_t n = value;
  while (1) {
    uint8_t byte = (uint8_t) (n & 0x7fu);
    n >>= 7u;
    if (n == 0u) {
      out[cursor++] = byte;
      return cursor;
    }
    out[cursor++] = (uint8_t) (byte | 0x80u);
  }
}

static uint32_t append_var_u32(uint8_t *dst, uint32_t cursor, uint32_t value) {
  uint8_t bytes[5];
  uint32_t len = encode_var_u32_bytes(value, bytes);
  for (uint32_t i = 0; i < len; i += 1u) {
    dst[cursor + i] = bytes[i];
  }
  return cursor + len;
}

static uint32_t base64_encoded_len(uint32_t raw_len) {
  return ((raw_len + 2u) / 3u) * 4u;
}

static Segment encode_base64_segment(Segment raw) {
  if (!raw.ok) {
    return missing_segment();
  }
  uint32_t out_len = base64_encoded_len(raw.len);
  uint32_t out_ptr = alloc_bytes(out_len, 1u);
  if (out_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *src = (uint8_t *) (uintptr_t) raw.ptr;
  uint8_t *dst = (uint8_t *) (uintptr_t) out_ptr;
  uint32_t src_at = 0u;
  uint32_t dst_at = 0u;
  while (src_at + 3u <= raw.len) {
    uint32_t block = ((uint32_t) src[src_at] << 16u) |
      ((uint32_t) src[src_at + 1u] << 8u) |
      (uint32_t) src[src_at + 2u];
    dst[dst_at + 0u] = (uint8_t) BASE64_ALPHABET[(block >> 18u) & 63u];
    dst[dst_at + 1u] = (uint8_t) BASE64_ALPHABET[(block >> 12u) & 63u];
    dst[dst_at + 2u] = (uint8_t) BASE64_ALPHABET[(block >> 6u) & 63u];
    dst[dst_at + 3u] = (uint8_t) BASE64_ALPHABET[block & 63u];
    src_at += 3u;
    dst_at += 4u;
  }
  if (src_at < raw.len) {
    uint32_t block = (uint32_t) src[src_at] << 16u;
    dst[dst_at + 0u] = (uint8_t) BASE64_ALPHABET[(block >> 18u) & 63u];
    if (src_at + 1u < raw.len) {
      block |= (uint32_t) src[src_at + 1u] << 8u;
      dst[dst_at + 1u] = (uint8_t) BASE64_ALPHABET[(block >> 12u) & 63u];
      dst[dst_at + 2u] = (uint8_t) BASE64_ALPHABET[(block >> 6u) & 63u];
      dst[dst_at + 3u] = '=';
    } else {
      dst[dst_at + 1u] = (uint8_t) BASE64_ALPHABET[(block >> 12u) & 63u];
      dst[dst_at + 2u] = '=';
      dst[dst_at + 3u] = '=';
    }
  }
  Segment out;
  out.ptr = out_ptr;
  out.len = out_len;
  out.ok = 1;
  return out;
}

static Segment clone_segment(Segment seg) {
  if (!seg.ok || seg.len == 0u) {
    return missing_segment();
  }
  uint32_t copied_ptr = alloc_bytes(seg.len, 1u);
  if (copied_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) copied_ptr;
  uint8_t *src = (uint8_t *) (uintptr_t) seg.ptr;
  for (uint32_t i = 0; i < seg.len; i += 1) {
    dst[i] = src[i];
  }
  Segment out;
  out.ptr = copied_ptr;
  out.len = seg.len;
  out.ok = 1;
  return out;
}

static void write_wasm_base64(uint8_t *dst, uint32_t *cursor,
  const char *wasm_base64,
  uint32_t wasm_base64_len) {
  uint8_t *dst_ptr = dst + *cursor;
  const uint8_t *src = (const uint8_t *) (uintptr_t) wasm_base64;
  for (uint32_t i = wasm_base64_len; i > 0; i -= 1u) {
    dst_ptr[i - 1u] = src[i - 1u];
  }
  *cursor += wasm_base64_len;
}

static void write_seed_base64(uint8_t *dst, uint32_t *cursor) {
  write_wasm_base64(dst, cursor, SEED_WASM_BASE64, SEED_WASM_BASE64_LEN);
}

static int is_ident_start(uint8_t b) {
  return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || b == '_';
}

static int is_ident_continue(uint8_t b) {
  return is_ident_start(b) || (b >= '0' && b <= '9') || b == '\'';
}

static int is_operator_head(uint8_t b) {
  return b == '!' || b == '#' || b == '$' || b == '%' || b == '&' ||
    b == '*' || b == '+' || b == '.' || b == '/' || b == ':' || b == '<' ||
    b == '>' || b == '?' || b == '@' || b == '\\' || b == '^' || b == '|' ||
    b == '~';
}

static int is_operator_continue(uint8_t b) {
  return is_operator_head(b) || b == '-' || b == '=';
}

static int is_operator_start(uint8_t b) {
  return is_operator_head(b) || b == '-' || b == '=';
}

static int names_equal(NameSpan left, NameSpan right) {
  if (!left.ok || !right.ok || left.len != right.len) {
    return 0;
  }
  uint8_t *l = (uint8_t *) (uintptr_t) left.ptr;
  uint8_t *r = (uint8_t *) (uintptr_t) right.ptr;
  for (uint32_t i = 0; i < left.len; i += 1) {
    if (l[i] != r[i]) {
      return 0;
    }
  }
  return 1;
}

static int namespan_equals_literal(NameSpan name, const char *literal) {
  NameSpan rhs;
  rhs.ptr = (uint32_t) (uintptr_t) literal;
  rhs.len = cstr_len(literal);
  rhs.ok = 1;
  return names_equal(name, rhs);
}

static int roots_contains(NameSpan name, NameSpan *roots, uint32_t roots_count) {
  for (uint32_t i = 0; i < roots_count; i += 1) {
    if (names_equal(name, roots[i])) {
      return 1;
    }
  }
  return 0;
}

static uint32_t roots_push_unique(NameSpan name, NameSpan *roots, uint32_t roots_count) {
  if (!name.ok || name.len == 0u || roots_count >= MAX_ROOTS) {
    return roots_count;
  }
  if (roots_contains(name, roots, roots_count)) {
    return roots_count;
  }
  roots[roots_count] = name;
  return roots_count + 1u;
}

static uint32_t source_line_end(Segment source, uint32_t start) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t i = start;
  while (i < source.len) {
    uint8_t b0 = mem[i];
    if (b0 == '\n') {
      return i;
    }
    i += 1u;
  }
  return source.len;
}

static uint32_t source_next_line_start(Segment source, uint32_t line_end) {
  if (line_end >= source.len) {
    return source.len;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (mem[line_end] == '\n') {
    return line_end + 1u;
  }
  return source.len;
}

static uint32_t extend_lambda_truncated_line_end(
  Segment source,
  uint32_t start,
  uint32_t line_end
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (line_end <= start || line_end >= source.len) {
    return line_end;
  }
  if (mem[line_end] != '\\' || line_end + 1u >= source.len ||
      !is_ident_start(mem[line_end + 1u])) {
    return line_end;
  }
  uint32_t cursor = line_end + 2u;
  while (cursor < source.len && mem[cursor] == ' ') {
    cursor += 1u;
  }
  if (cursor + 1u >= source.len || mem[cursor] != '-' || mem[cursor + 1u] != '>') {
    return line_end;
  }
  while (line_end < source.len && mem[line_end] != '\n') {
    line_end += 1u;
  }
  return line_end;
}

static uint32_t source_skip_line_ws(Segment source, uint32_t at, uint32_t line_end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t i = at;
  while (i < line_end && (mem[i] == ' ' || mem[i] == '\t')) {
    i += 1u;
  }
  return i;
}

static uint32_t source_parse_ident_end(Segment source, uint32_t at, uint32_t line_end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t i = at;
  while (i < line_end && is_ident_continue(mem[i])) {
    i += 1u;
  }
  return i;
}

static uint32_t source_parse_operator_end(Segment source, uint32_t at, uint32_t line_end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (at >= line_end) {
    return at;
  }
  uint8_t b0 = mem[at];
  if (b0 == '=') {
    if (at + 1u >= line_end || !is_operator_continue(mem[at + 1u])) {
      return at;
    }
    at += 2u;
  } else if (b0 == '-') {
    if (at + 1u >= line_end || !is_operator_continue(mem[at + 1u])) {
      return at + 1u;
    }
    at += 2u;
  } else if (is_operator_head(b0)) {
    at += 1u;
  } else {
    return at;
  }
  while (at < line_end && is_operator_continue(mem[at])) {
    at += 1u;
  }
  return at;
}

static int is_root_name_span_valid(NameSpan name) {
  if (!name.ok || name.len == 0u) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) name.ptr;
  uint8_t b0 = mem[0];
  if (is_ident_start(b0)) {
    for (uint32_t i = 1u; i < name.len; i += 1u) {
      if (!is_ident_continue(mem[i])) {
        return 0;
      }
    }
    return 1;
  }
  if (b0 == '=') {
    if (name.len < 2u || !is_operator_continue(mem[1u])) {
      return 0;
    }
    for (uint32_t i = 2u; i < name.len; i += 1u) {
      if (!is_operator_continue(mem[i])) {
        return 0;
      }
    }
    return 1;
  }
  if (b0 == '-') {
    if (name.len == 1u) {
      return 1;
    }
    if (!is_operator_continue(mem[1u])) {
      return 0;
    }
    for (uint32_t i = 2u; i < name.len; i += 1u) {
      if (!is_operator_continue(mem[i])) {
        return 0;
      }
    }
    return 1;
  }
  if (is_operator_head(b0)) {
    for (uint32_t i = 1u; i < name.len; i += 1u) {
      if (!is_operator_continue(mem[i])) {
        return 0;
      }
    }
    return 1;
  }
  return 0;
}

static int is_keyword_name(NameSpan name) {
  return namespan_equals_literal(name, "module") ||
    namespan_equals_literal(name, "import") ||
    namespan_equals_literal(name, "export") ||
    namespan_equals_literal(name, "data") ||
    namespan_equals_literal(name, "type") ||
    namespan_equals_literal(name, "class") ||
    namespan_equals_literal(name, "instance") ||
    namespan_equals_literal(name, "literal") ||
    namespan_equals_literal(name, "law");
}

static int parse_temp_identifier(const uint8_t *mem, uint32_t at, uint32_t end, uint32_t *temp_index_out) {
  if (at >= end || mem[at] != 't') {
    return 0;
  }
  uint32_t i = at + 1u;
  if (i >= end) {
    return 0;
  }
  uint8_t d0 = mem[i];
  if (d0 < '0' || d0 > '9') {
    return 0;
  }
  uint32_t temp_index = 0u;
  while (i < end) {
    uint8_t d = mem[i];
    if (d < '0' || d > '9') {
      break;
    }
    temp_index = temp_index * 10u + (uint32_t) (d - '0');
    i += 1u;
  }
  if (i != end) {
    return 0;
  }
  *temp_index_out = temp_index;
  return 1;
}

static int temp_slot_index(uint32_t temp_index, const uint32_t *slots, uint32_t slot_count) {
  for (uint32_t i = 0u; i < slot_count; i += 1u) {
    if (slots[i] == temp_index) {
      return (int) i;
    }
  }
  return -1;
}

static int ensure_temp_slot(
  uint32_t temp_index,
  uint32_t *slots,
  int *live,
  uint32_t *slot_count
) {
  int slot = temp_slot_index(temp_index, slots, *slot_count);
  if (slot >= 0) {
    return slot;
  }
  if (*slot_count >= MAX_TEMP_BINDINGS) {
    return -1;
  }
  slots[*slot_count] = temp_index;
  live[*slot_count] = 0;
  *slot_count += 1u;
  return (int) (*slot_count - 1u);
}

static void mark_temp_uses(
  Segment source,
  uint32_t start,
  uint32_t end,
  uint32_t *slots,
  int *live,
  uint32_t *slot_count
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t i = start;
  while (i < end) {
    uint8_t b = mem[i];
    if (b == '-' && i + 1u < end && mem[i + 1u] == '-') {
      while (i < end && mem[i] != '\n') {
        i += 1u;
      }
    }
    if (b == '"') {
      uint32_t j = i + 1u;
      int escaped = 0;
      while (j < end) {
        uint8_t c = mem[j];
        if (escaped) {
          escaped = 0;
          j += 1u;
          continue;
        }
        if (c == '\\') {
          escaped = 1;
          j += 1u;
          continue;
        }
        if (c == '"') {
          j += 1u;
          break;
        }
        j += 1u;
      }
      i = j;
      continue;
    }
    if (!is_ident_start(b)) {
      i += 1u;
      continue;
    }
    uint32_t token_end = source_parse_ident_end(source, i, end);
    if (token_end > i) {
      uint32_t temp_index = 0u;
      if (parse_temp_identifier(mem, i, token_end, &temp_index)) {
        int slot = ensure_temp_slot(temp_index, slots, live, slot_count);
        if (slot >= 0) {
          live[slot] = 1;
        }
      }
    }
    i = token_end;
  }
}

static int parse_temp_binding_line(
  Segment source,
  uint32_t line_start,
  uint32_t content_end,
  uint32_t *temp_index_out,
  uint32_t *rhs_start_out,
  uint32_t *rhs_end_out
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t at = source_skip_line_ws(source, line_start, content_end);
  if (at + 3u > content_end || mem[at] != 'l' || mem[at + 1u] != 'e' || mem[at + 2u] != 't') {
    return 0;
  }
  if (at + 3u < content_end && is_ident_continue(mem[at + 3u])) {
    return 0;
  }
  uint32_t name_start = source_skip_line_ws(source, at + 3u, content_end);
  if (name_start >= content_end) {
    return 0;
  }
  uint32_t name_end = source_parse_ident_end(source, name_start, content_end);
  if (name_end <= name_start) {
    return 0;
  }
  uint32_t temp_index = 0u;
  if (!parse_temp_identifier(mem, name_start, name_end, &temp_index)) {
    return 0;
  }
  uint32_t eq_at = name_end;
  while (eq_at < content_end && mem[eq_at] != '=') {
    eq_at += 1u;
  }
  if (eq_at >= content_end) {
    return 0;
  }
  uint32_t rhs_start = source_skip_line_ws(source, eq_at + 1u, content_end);
  if (rhs_start >= content_end) {
    return 0;
  }
  *temp_index_out = temp_index;
  *rhs_start_out = rhs_start;
  *rhs_end_out = content_end;
  return 1;
}

static void write_temp_name(uint8_t *dst, uint32_t *cursor, uint32_t temp_index) {
  dst[*cursor] = 't';
  *cursor += 1u;
  uint32_t digits[8];
  uint32_t digit_count = 0u;
  uint32_t value = temp_index;
  do {
    digits[digit_count] = value % 10u;
    digit_count += 1u;
    value /= 10u;
  } while (value > 0u);
  while (digit_count > 0u) {
    digit_count -= 1u;
    dst[*cursor] = (uint8_t) ('0' + digits[digit_count]);
    *cursor += 1u;
  }
}

static int rewrite_function_temp_lines(
  Segment source,
  uint32_t body_start,
  uint32_t body_end,
  uint32_t *cursor,
  uint8_t *dst
) {
  uint8_t *src = (uint8_t *) (uintptr_t) source.ptr;
  TempLine *lines = temp_lines_workspace;
  uint32_t line_count = 0u;
  int overflowed = 0;

  uint32_t line_start = body_start;
  while (line_start < body_end) {
    if (line_count >= MAX_TEMP_LINES) {
      overflowed = 1;
      break;
    }
    uint32_t content_end = source_line_end(source, line_start);
    uint32_t line_end = source_next_line_start(source, content_end);

    lines[line_count].line_start = line_start;
    lines[line_count].line_end = line_end;
    lines[line_count].content_end = content_end;
    lines[line_count].is_temp_binding = 0;
    lines[line_count].keep_line = 1;
    lines[line_count].old_temp = 0u;
    lines[line_count].rhs_start = 0u;
    lines[line_count].rhs_end = 0u;

    uint32_t temp_index = 0u;
    uint32_t rhs_start = 0u;
    uint32_t rhs_end = 0u;
    if (parse_temp_binding_line(source, line_start, content_end, &temp_index, &rhs_start, &rhs_end)) {
      lines[line_count].is_temp_binding = 1;
      lines[line_count].old_temp = temp_index;
      lines[line_count].rhs_start = rhs_start;
      lines[line_count].rhs_end = rhs_end;
    }
    line_count += 1u;
    line_start = line_end;
  }
  if (overflowed || line_start < body_end) {
    for (uint32_t at = body_start; at < body_end; at += 1u) {
      dst[*cursor] = src[at];
      *cursor += 1u;
    }
    return 1;
  }

  uint32_t *temp_slots = temp_slots_workspace;
  int *temp_live = temp_live_workspace;
  uint32_t temp_slot_count = 0u;
  for (uint32_t i = 0u; i < MAX_TEMP_BINDINGS; i += 1u) {
    temp_slots[i] = 0u;
    temp_live[i] = 0;
  }

  for (uint32_t idx = line_count; idx > 0u; idx -= 1u) {
    uint32_t li = idx - 1u;
    TempLine *line = &lines[li];
    if (line->is_temp_binding) {
      int slot = temp_slot_index(line->old_temp, temp_slots, temp_slot_count);
      line->keep_line = slot >= 0 && temp_live[slot] == 1;
      if (line->keep_line) {
        mark_temp_uses(source, line->rhs_start, line->rhs_end, temp_slots, temp_live, &temp_slot_count);
      }
    } else {
      mark_temp_uses(source, line->line_start, line->content_end, temp_slots, temp_live, &temp_slot_count);
    }
  }

  uint32_t *renumber_slots = renumber_slots_workspace;
  uint32_t renumber_count = 0u;
  for (uint32_t i = 0u; i < line_count; i += 1u) {
    TempLine *line = &lines[i];
    if (!line->is_temp_binding || !line->keep_line) {
      continue;
    }
    if (temp_slot_index(line->old_temp, renumber_slots, renumber_count) < 0) {
      if (renumber_count >= MAX_TEMP_BINDINGS) {
        for (uint32_t at = body_start; at < body_end; at += 1u) {
          dst[*cursor] = src[at];
          *cursor += 1u;
        }
        return 1;
      }
      renumber_slots[renumber_count] = line->old_temp;
      renumber_count += 1u;
    }
  }

  for (uint32_t i = 0u; i < line_count; i += 1u) {
    TempLine *line = &lines[i];
    if (line->is_temp_binding && !line->keep_line) {
      continue;
    }
    for (uint32_t at = line->line_start; at < line->line_end; ) {
      if (src[at] == '-' && at + 1u < line->line_end && src[at + 1u] == '-') {
        while (at < line->line_end) {
          dst[*cursor] = src[at];
          *cursor += 1u;
          at += 1u;
        }
        continue;
      }
      if (src[at] == '"') {
        uint32_t j = at;
        int escaped = 0;
        while (j < line->line_end) {
          uint8_t c = src[j];
          dst[*cursor] = c;
          *cursor += 1u;
          if (escaped) {
            escaped = 0;
            j += 1u;
            continue;
          }
          if (c == '\\') {
            escaped = 1;
            j += 1u;
            continue;
          }
          if (c == '"') {
            j += 1u;
            break;
          }
          j += 1u;
        }
        at = j;
        continue;
      }
      if (!is_ident_start(src[at])) {
        dst[*cursor] = src[at];
        *cursor += 1u;
        at += 1u;
        continue;
      }
      uint32_t token_end = source_parse_ident_end(source, at, line->line_end);
      if (token_end <= at) {
        dst[*cursor] = src[at];
        *cursor += 1u;
        at += 1u;
        continue;
      }
      uint32_t temp_index = 0u;
      if (parse_temp_identifier(src, at, token_end, &temp_index)) {
        int renumber_slot = temp_slot_index(temp_index, renumber_slots, renumber_count);
        if (renumber_slot >= 0) {
          write_temp_name(dst, cursor, (uint32_t) renumber_slot);
        } else {
          for (uint32_t j = at; j < token_end; j += 1u) {
            dst[*cursor] = src[j];
            *cursor += 1u;
          }
        }
      } else {
        for (uint32_t j = at; j < token_end; j += 1u) {
          dst[*cursor] = src[j];
          *cursor += 1u;
        }
      }
      at = token_end;
    }
  }

  return 1;
}

static Segment build_temp_pruned_segment(Segment source) {
  FnDecl *decls = fn_decls_workspace;
  uint32_t decl_count = collect_fn_decls(source, decls, MAX_FN_DECLS);
  if (decl_count == 0u) {
    return source;
  }

  /*
   * Temp renumbering may widen tokens (for example t9 -> t10), so the
   * rewritten function body can be larger than the input segment.
   * Reserve headroom to avoid response-buffer corruption.
   */
  uint32_t out_capacity = source.len;
  if (out_capacity <= (UINT32_MAX - 64u) / 2u) {
    out_capacity = out_capacity * 2u + 64u;
  }

  uint32_t out_ptr = alloc_bytes(out_capacity, 1u);
  if (out_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *src = (uint8_t *) (uintptr_t) source.ptr;
  uint8_t *dst = (uint8_t *) (uintptr_t) out_ptr;
  uint32_t cursor = 0u;

  uint32_t copy_at = 0u;
  for (uint32_t i = 0u; i < decl_count; i += 1u) {
    FnDecl decl = decls[i];
    uint32_t function_end = (i + 1u < decl_count) ? decls[i + 1u].line_start : source.len;
    for (uint32_t j = copy_at; j < decl.line_start; j += 1u) {
      dst[cursor] = src[j];
      cursor += 1u;
    }

    for (uint32_t j = decl.line_start; j < decl.body_start; j += 1u) {
      dst[cursor] = src[j];
      cursor += 1u;
    }

    if (!rewrite_function_temp_lines(source, decl.body_start, function_end, &cursor, dst)) {
      return missing_segment();
    }

    copy_at = function_end;
  }

  for (uint32_t j = copy_at; j < source.len; j += 1u) {
    dst[cursor] = src[j];
    cursor += 1u;
  }

  Segment out;
  out.ptr = out_ptr;
  out.len = cursor;
  out.ok = 1;
  return out;
}

static int parse_guarded_top_level_decl(
  Segment source,
  uint32_t line_start,
  uint32_t line_end,
  uint32_t next_line,
  FnDecl *out
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t trimmed = source_skip_line_ws(source, line_start, line_end);
  if (trimmed != line_start || trimmed >= line_end) {
    return 0;
  }
  if (mem[trimmed] == '-' && trimmed + 1u < line_end && mem[trimmed + 1u] == '-') {
    return 0;
  }
  uint8_t b0 = mem[trimmed];
  uint32_t name_end = 0u;
  if (is_ident_start(b0)) {
    name_end = source_parse_ident_end(source, trimmed, line_end);
  } else if (is_operator_start(b0)) {
    name_end = source_parse_operator_end(source, trimmed, line_end);
  } else {
    return 0;
  }
  if (name_end <= trimmed) {
    return 0;
  }
  NameSpan name;
  name.ptr = source.ptr + trimmed;
  name.len = name_end - trimmed;
  name.ok = name.len > 0u;
  if (!name.ok) {
    return 0;
  }
  if (is_ident_start(b0) && is_keyword_name(name)) {
    return 0;
  }
  if (next_line >= source.len) {
    return 0;
  }
  {
    uint32_t next_end = source_line_end(source, next_line);
    uint32_t next_trimmed = source_skip_line_ws(source, next_line, next_end);
    if (next_trimmed <= next_line || next_trimmed >= next_end || mem[next_trimmed] != '|') {
      return 0;
    }
  }
  out->name = name;
  out->line_start = line_start;
  out->line_end = line_end;
  out->body_start = next_line;
  out->body_end = line_end;
  return 1;
}

static int parse_top_level_decl(Segment source, uint32_t line_start, uint32_t line_end, uint32_t next_line, FnDecl *out) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t trimmed = source_skip_line_ws(source, line_start, line_end);
  if (trimmed != line_start || trimmed >= line_end) {
    return 0;
  }
  if (mem[trimmed] == '-' && trimmed + 1u < line_end && mem[trimmed + 1u] == '-') {
    return 0;
  }
  uint8_t b0 = mem[trimmed];
  uint32_t name_end = 0u;
  if (is_ident_start(b0)) {
    name_end = source_parse_ident_end(source, trimmed, line_end);
  } else if (is_operator_start(b0)) {
    name_end = source_parse_operator_end(source, trimmed, line_end);
  } else {
    return 0;
  }
  if (name_end <= trimmed) {
    return 0;
  }
  NameSpan name;
  name.ptr = source.ptr + trimmed;
  name.len = name_end - trimmed;
  name.ok = name.len > 0u;
  if (!name.ok) {
    return 0;
  }
  if (is_ident_start(b0) && is_keyword_name(name)) {
    return 0;
  }
  uint32_t eq_at = find_top_level_assignment_eq(source, name_end, line_end);
  if (eq_at >= line_end) {
    return parse_guarded_top_level_decl(source, line_start, line_end, next_line, out);
  }
  {
    uint32_t clause_bar = find_top_level_clause_bar(source, name_end, eq_at);
    if (clause_bar < eq_at) {
      out->name = name;
      out->line_start = line_start;
      out->line_end = line_end;
      out->body_start = clause_bar;
      out->body_end = line_end;
      return 1;
    }
  }
  uint32_t body_start = eq_at + 1u;
  while (body_start < line_end && (mem[body_start] == ' ' || mem[body_start] == '\t')) {
    body_start += 1u;
  }
  out->name = name;
  out->line_start = line_start;
  out->line_end = line_end;
  out->body_start = body_start;
  out->body_end = line_end;
  return 1;
}

static NameSpan missing_name_span(void) {
  NameSpan out;
  out.ptr = 0u;
  out.len = 0u;
  out.ok = 0;
  return out;
}

static NameSpan parse_body_head_call_name(Segment source, FnDecl decl) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t at = source_skip_line_ws(source, decl.body_start, decl.body_end);
  if (at >= decl.body_end) {
    return missing_name_span();
  }
  uint8_t b0 = mem[at];
  uint32_t end = 0u;
  if (is_ident_start(b0)) {
    end = source_parse_ident_end(source, at, decl.body_end);
  } else if (is_operator_start(b0)) {
    end = source_parse_operator_end(source, at, decl.body_end);
  } else {
    return missing_name_span();
  }
  if (end <= at) {
    return missing_name_span();
  }
  NameSpan name;
  name.ptr = source.ptr + at;
  name.len = end - at;
  name.ok = name.len > 0u;
  if (!name.ok) {
    return missing_name_span();
  }
  if (is_ident_start(b0) && is_keyword_name(name)) {
    return missing_name_span();
  }
  return name;
}

static int find_decl_index_by_name(FnDecl *decls, uint32_t decl_count, NameSpan name) {
  for (uint32_t i = 0; i < decl_count; i += 1u) {
    if (names_equal(name, decls[i].name)) {
      return (int) i;
    }
  }
  return -1;
}

static uint32_t decl_param_count(Segment source, FnDecl decl) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t cursor = (decl.name.ptr - source.ptr) + decl.name.len;
  uint32_t count = 0u;
  while (cursor < decl.line_end) {
    while (cursor < decl.line_end &&
           (mem[cursor] == ' ' || mem[cursor] == '\t')) {
      cursor += 1u;
    }
    if (cursor >= decl.line_end || mem[cursor] == '=' || mem[cursor] == '|') {
      break;
    }
    uint32_t end = cursor;
    if (is_ident_start(mem[cursor])) {
      end = source_parse_ident_end(source, cursor, decl.line_end);
    } else if (is_operator_start(mem[cursor])) {
      end = source_parse_operator_end(source, cursor, decl.line_end);
    } else {
      cursor += 1u;
      continue;
    }
    if (end > cursor) {
      count += 1u;
    }
    cursor = end;
  }
  return count;
}

static EvalConst missing_eval_const(void) {
  EvalConst out;
  out.ok = 0;
  out.value = 0;
  return out;
}

static EvalConst make_eval_const(int32_t value) {
  EvalConst out;
  out.ok = 1;
  out.value = value;
  return out;
}

static uint32_t skip_inline_ws(Segment source, uint32_t at, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  while (at < end && (mem[at] == ' ' || mem[at] == '\t')) {
    at += 1u;
  }
  return at;
}

static int parse_signed_int_literal(
  Segment source,
  uint32_t at,
  uint32_t end,
  int32_t *value_out,
  uint32_t *next_out
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (at >= end) {
    return 0;
  }
  int sign = 1;
  if (mem[at] == '-') {
    if (at + 1u >= end || mem[at + 1u] < '0' || mem[at + 1u] > '9') {
      return 0;
    }
    sign = -1;
    at += 1u;
  }
  if (at >= end || mem[at] < '0' || mem[at] > '9') {
    return 0;
  }
  int32_t value = 0;
  while (at < end && mem[at] >= '0' && mem[at] <= '9') {
    value = value * 10 + (int32_t) (mem[at] - '0');
    at += 1u;
  }
  *value_out = sign < 0 ? -value : value;
  *next_out = at;
  return 1;
}

static int is_assignment_equals_at(Segment source, uint32_t at, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (at < start || at >= end || mem[at] != '=') {
    return 0;
  }
  uint8_t prev = at > start ? mem[at - 1u] : 0u;
  uint8_t next = at + 1u < end ? mem[at + 1u] : 0u;
  if (prev == '=' || prev == '!' || prev == '<' || prev == '>') {
    return 0;
  }
  if (next == '=' || next == '>') {
    return 0;
  }
  return 1;
}

static uint32_t find_top_level_assignment_eq(Segment source, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = start; at < end; at += 1u) {
    uint8_t c = mem[at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == '(') {
      depth += 1u;
      continue;
    }
    if (c == ')' && depth > 0u) {
      depth -= 1u;
      continue;
    }
    if (depth == 0u && is_assignment_equals_at(source, at, start, end)) {
      return at;
    }
  }
  return end;
}

static int is_clause_bar_at(Segment source, uint32_t at, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (at < start || at >= end || mem[at] != '|') {
    return 0;
  }
  uint8_t prev = at > start ? mem[at - 1u] : 0u;
  uint8_t next = at + 1u < end ? mem[at + 1u] : 0u;
  return prev != '|' && next != '|';
}

static uint32_t find_top_level_clause_bar(Segment source, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = start; at < end; at += 1u) {
    uint8_t c = mem[at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == '(') {
      depth += 1u;
      continue;
    }
    if (c == ')' && depth > 0u) {
      depth -= 1u;
      continue;
    }
    if (depth == 0u && is_clause_bar_at(source, at, start, end)) {
      return at;
    }
  }
  return end;
}

static NameSpan parse_simple_name_token(Segment source, uint32_t at, uint32_t end, uint32_t *next_out) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  NameSpan out = missing_name_span();
  if (at >= end) {
    return out;
  }
  uint32_t name_end = at;
  if (is_ident_start(mem[at])) {
    name_end = source_parse_ident_end(source, at, end);
  } else if (is_operator_start(mem[at])) {
    name_end = source_parse_operator_end(source, at, end);
  } else {
    return out;
  }
  if (name_end <= at) {
    return out;
  }
  out.ptr = source.ptr + at;
  out.len = name_end - at;
  out.ok = 1;
  *next_out = name_end;
  return out;
}

static uint32_t trim_expr_end(Segment source, uint32_t start, uint32_t end);
static uint32_t skip_expr_ws(Segment source, uint32_t at, uint32_t end);

static int span_is_exact_simple_name(Segment source, uint32_t start, uint32_t end, NameSpan expected) {
  uint32_t cursor = skip_expr_ws(source, start, end);
  uint32_t limit = trim_expr_end(source, cursor, end);
  uint32_t next = cursor;
  NameSpan token = parse_simple_name_token(source, cursor, limit, &next);
  return token.ok && next == limit && names_equal(token, expected);
}

static uint32_t collect_decl_params(
  Segment source,
  FnDecl decl,
  NameSpan *params,
  uint32_t max_params
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t cursor = (decl.name.ptr - source.ptr) + decl.name.len;
  uint32_t count = 0u;
  while (cursor < decl.line_end) {
    while (cursor < decl.line_end &&
           (mem[cursor] == ' ' || mem[cursor] == '\t')) {
      cursor += 1u;
    }
    if (cursor >= decl.line_end || mem[cursor] == '=' || mem[cursor] == '|') {
      break;
    }
    uint32_t end = cursor;
    if (is_ident_start(mem[cursor])) {
      end = source_parse_ident_end(source, cursor, decl.line_end);
    } else if (is_operator_start(mem[cursor])) {
      end = source_parse_operator_end(source, cursor, decl.line_end);
    } else {
      cursor += 1u;
      continue;
    }
    if (end > cursor && count < max_params) {
      params[count].ptr = source.ptr + cursor;
      params[count].len = end - cursor;
      params[count].ok = 1;
      count += 1u;
    }
    cursor = end;
  }
  return count;
}

static EvalConst lookup_eval_param(
  NameSpan name,
  NameSpan *params,
  EvalConst *values,
  uint32_t count
) {
  for (uint32_t i = 0u; i < count; i += 1u) {
    if (names_equal(name, params[i]) && values[i].ok) {
      return values[i];
    }
  }
  return missing_eval_const();
}

static EvalConst apply_eval_builtin(NameSpan name, EvalConst *args, uint32_t argc) {
  if (namespan_equals_literal(name, "id") && argc == 1u && args[0].ok) {
    return args[0];
  }
  if (namespan_equals_literal(name, "add") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value + args[1].value);
  }
  if (namespan_equals_literal(name, "sub") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value - args[1].value);
  }
  if (namespan_equals_literal(name, "mul") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value * args[1].value);
  }
  if (namespan_equals_literal(name, "div") && argc == 2u && args[0].ok && args[1].ok &&
      args[1].value != 0) {
    return make_eval_const(args[0].value / args[1].value);
  }
  if (namespan_equals_literal(name, "mod") && argc == 2u && args[0].ok && args[1].ok &&
      args[1].value != 0) {
    return make_eval_const(args[0].value % args[1].value);
  }
  if (namespan_equals_literal(name, "eq") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value == args[1].value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "ne") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value != args[1].value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "lt") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value < args[1].value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "le") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value <= args[1].value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "gt") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value > args[1].value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "ge") && argc == 2u && args[0].ok && args[1].ok) {
    return make_eval_const(args[0].value >= args[1].value ? 1 : 0);
  }
  return missing_eval_const();
}

static EvalConst eval_decl_body_simple(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  FnDecl decl,
  NameSpan *params,
  EvalConst *param_values,
  uint32_t param_count,
  uint32_t depth
);

static EvalConst eval_decl_by_name_simple(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan name,
  EvalConst *args,
  uint32_t argc,
  uint32_t depth
) {
  if (depth > 32u) {
    return missing_eval_const();
  }
  int decl_index = find_decl_index_by_name(decls, decl_count, name);
  if (decl_index < 0) {
    return missing_eval_const();
  }
  FnDecl decl = decls[(uint32_t) decl_index];
  NameSpan params[16];
  uint32_t param_count = collect_decl_params(source, decl, params, 16u);
  if (param_count != argc) {
    return missing_eval_const();
  }
  return eval_decl_body_simple(source, decls, decl_count, decl, params, args, argc, depth + 1u);
}

static EvalConst parse_simple_atom(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t *cursor_io,
  uint32_t end,
  NameSpan *params,
  EvalConst *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  uint32_t cursor = skip_inline_ws(source, *cursor_io, end);
  int32_t int_value = 0;
  uint32_t next = cursor;
  if (parse_signed_int_literal(source, cursor, end, &int_value, &next)) {
    *cursor_io = next;
    return make_eval_const(int_value);
  }
  NameSpan name = parse_simple_name_token(source, cursor, end, &next);
  if (!name.ok) {
    return missing_eval_const();
  }
  *cursor_io = next;
  if (namespan_equals_literal(name, "true") || namespan_equals_literal(name, "True")) {
    return make_eval_const(1);
  }
  if (namespan_equals_literal(name, "false") || namespan_equals_literal(name, "False")) {
    return make_eval_const(0);
  }
  EvalConst param_value = lookup_eval_param(name, params, param_values, param_count);
  if (param_value.ok) {
    return param_value;
  }
  return eval_decl_by_name_simple(source, decls, decl_count, name, NULL, 0u, depth + 1u);
}

static EvalConst eval_decl_body_simple(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  FnDecl decl,
  NameSpan *params,
  EvalConst *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  if (depth > 32u) {
    return missing_eval_const();
  }
  uint32_t cursor = skip_inline_ws(source, decl.body_start, decl.body_end);
  uint32_t head_next = cursor;
  int32_t int_value = 0;
  if (parse_signed_int_literal(source, cursor, decl.body_end, &int_value, &head_next)) {
    head_next = skip_inline_ws(source, head_next, decl.body_end);
    return head_next == decl.body_end ? make_eval_const(int_value) : missing_eval_const();
  }
  NameSpan head = parse_simple_name_token(source, cursor, decl.body_end, &head_next);
  if (!head.ok) {
    return missing_eval_const();
  }
  uint32_t tail = skip_inline_ws(source, head_next, decl.body_end);
  if (tail == decl.body_end) {
    if (namespan_equals_literal(head, "true") || namespan_equals_literal(head, "True")) {
      return make_eval_const(1);
    }
    if (namespan_equals_literal(head, "false") || namespan_equals_literal(head, "False")) {
      return make_eval_const(0);
    }
    EvalConst param_value = lookup_eval_param(head, params, param_values, param_count);
    if (param_value.ok) {
      return param_value;
    }
    return eval_decl_by_name_simple(source, decls, decl_count, head, NULL, 0u, depth + 1u);
  }
  EvalConst args[8];
  uint32_t argc = 0u;
  while (tail < decl.body_end && argc < 8u) {
    EvalConst arg = parse_simple_atom(
      source,
      decls,
      decl_count,
      &tail,
      decl.body_end,
      params,
      param_values,
      param_count,
      depth + 1u
    );
    if (!arg.ok) {
      return missing_eval_const();
    }
    args[argc++] = arg;
    tail = skip_inline_ws(source, tail, decl.body_end);
  }
  if (tail != decl.body_end) {
    return missing_eval_const();
  }
  EvalConst builtin = apply_eval_builtin(head, args, argc);
  if (builtin.ok) {
    return builtin;
  }
  return eval_decl_by_name_simple(source, decls, decl_count, head, args, argc, depth + 1u);
}

static EvalConst eval_root_simple(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan root
) {
  return eval_decl_by_name_simple(source, decls, decl_count, root, NULL, 0u, 0u);
}

static EvalValue missing_eval_value(void) {
  EvalValue out;
  out.ok = 0;
  out.kind = 0u;
  out.int_value = 0;
  out.slice_len = 0u;
  for (uint32_t i = 0u; i < MAX_EVAL_SLICE_BYTES; i += 1u) {
    out.slice_bytes[i] = 0u;
  }
  return out;
}

static EvalValue make_eval_int_value(int32_t value) {
  EvalValue out = missing_eval_value();
  out.ok = 1;
  out.kind = EVAL_VALUE_INT;
  out.int_value = value;
  return out;
}

static EvalValue make_eval_slice_value(uint32_t len) {
  EvalValue out = missing_eval_value();
  if (len > MAX_EVAL_SLICE_BYTES) {
    return out;
  }
  out.ok = 1;
  out.kind = EVAL_VALUE_SLICE;
  out.slice_len = len;
  for (uint32_t i = 0u; i < len; i += 1u) {
    out.slice_bytes[i] = 0u;
  }
  return out;
}

static EvalConst eval_const_from_value(EvalValue value) {
  if (!value.ok || value.kind != EVAL_VALUE_INT) {
    return missing_eval_const();
  }
  return make_eval_const(value.int_value);
}

static int eval_value_is_int(EvalValue value) {
  return value.ok && value.kind == EVAL_VALUE_INT;
}

static uint32_t trim_expr_end(Segment source, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  while (end > start) {
    uint8_t c = mem[end - 1u];
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
      end -= 1u;
      continue;
    }
    break;
  }
  return end;
}

static uint32_t skip_expr_ws(Segment source, uint32_t at, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  while (at < end) {
    if (
      mem[at] == ' ' || mem[at] == '\t' || mem[at] == '\r' ||
      mem[at] == '\n'
    ) {
      at += 1u;
      continue;
    }
    if (mem[at] == '-' && at + 1u < end && mem[at + 1u] == '-') {
      while (at < end && mem[at] != '\n') {
        at += 1u;
      }
      continue;
    }
    break;
  }
  return at;
}

static int span_matches_keyword(
  Segment source,
  uint32_t start,
  uint32_t end,
  const char *keyword
) {
  uint32_t keyword_len = cstr_len(keyword);
  if (end < start || end - start < keyword_len) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  for (uint32_t i = 0u; i < keyword_len; i += 1u) {
    if (mem[start + i] != (uint8_t) keyword[i]) {
      return 0;
    }
  }
  if (start + keyword_len < end && is_ident_continue(mem[start + keyword_len])) {
    return 0;
  }
  return 1;
}

static int span_is_wrapped_parens(Segment source, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (end <= start || mem[start] != '(' || mem[end - 1u] != ')') {
    return 0;
  }
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = start; at < end; at += 1u) {
    uint8_t c = mem[at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == '(') {
      depth += 1u;
      continue;
    }
    if (c == ')') {
      if (depth == 0u) {
        return 0;
      }
      depth -= 1u;
      if (depth == 0u && at + 1u < end) {
        return 0;
      }
    }
  }
  return depth == 0u && !in_string;
}

static uint32_t parse_string_literal_end(Segment source, uint32_t at, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  int escaped_quotes = 0;
  if (at + 1u < end && mem[at] == '\\' && mem[at + 1u] == '"') {
    escaped_quotes = 1;
    at += 1u;
  }
  if (at >= end || mem[at] != '"') {
    return at;
  }
  at += 1u;
  int escaped = 0;
  while (at < end) {
    uint8_t c = mem[at];
    if (escaped) {
      escaped = 0;
      at += 1u;
      continue;
    }
    if (c == '\\') {
      if (escaped_quotes && at + 1u < end && mem[at + 1u] == '"') {
        return at + 2u;
      }
      escaped = 1;
      at += 1u;
      continue;
    }
    if (c == '"') {
      return at + 1u;
    }
    at += 1u;
  }
  return at;
}

static uint32_t parse_expr_atom_end(Segment source, uint32_t at, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  at = skip_expr_ws(source, at, end);
  if (at >= end) {
    return at;
  }
  if (mem[at] == '(') {
    uint32_t depth = 0u;
    int in_string = 0;
    int escaped = 0;
    uint32_t cursor = at;
    while (cursor < end) {
      uint8_t c = mem[cursor];
      if (in_string) {
        if (escaped) {
          escaped = 0;
        } else if (c == '\\') {
          escaped = 1;
        } else if (c == '"') {
          in_string = 0;
        }
        cursor += 1u;
        continue;
      }
      if (c == '"') {
        in_string = 1;
        cursor += 1u;
        continue;
      }
      if (c == '(') {
        depth += 1u;
      } else if (c == ')') {
        if (depth == 0u) {
          return cursor;
        }
        depth -= 1u;
        if (depth == 0u) {
          return cursor + 1u;
        }
      }
      cursor += 1u;
    }
    return cursor;
  }
  if (mem[at] == '"' || (at + 1u < end && mem[at] == '\\' && mem[at + 1u] == '"')) {
    return parse_string_literal_end(source, at, end);
  }
  {
    int32_t ignored = 0;
    uint32_t next = at;
    if (parse_signed_int_literal(source, at, end, &ignored, &next)) {
      return next;
    }
  }
  {
    uint32_t next = at;
    NameSpan token = parse_simple_name_token(source, at, end, &next);
    if (token.ok) {
      return next;
    }
  }
  return at;
}

static int parse_lambda_expr(
  Segment source,
  uint32_t start,
  uint32_t end,
  NameSpan *params,
  uint32_t max_params,
  uint32_t *param_count_out,
  uint32_t *body_start_out
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  start = skip_expr_ws(source, start, end);
  end = trim_expr_end(source, start, end);
  if (start >= end || mem[start] != '\\') {
    return 0;
  }
  uint32_t cursor = skip_expr_ws(source, start + 1u, end);
  uint32_t param_count = 0u;
  while (cursor < end) {
    if (param_count >= max_params) {
      return 0;
    }
    uint32_t next = cursor;
    NameSpan param = parse_simple_name_token(source, cursor, end, &next);
    if (!param.ok) {
      return 0;
    }
    params[param_count] = param;
    param_count += 1u;
    cursor = skip_expr_ws(source, next, end);
    if (cursor + 1u < end && mem[cursor] == '-' && mem[cursor + 1u] == '>') {
      uint32_t body_start = skip_expr_ws(source, cursor + 2u, end);
      if (body_start > end) {
        return 0;
      }
      *param_count_out = param_count;
      *body_start_out = body_start;
      return 1;
    }
  }
  return 0;
}

static uint32_t decl_function_end(
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t decl_index,
  uint32_t source_len
) {
  if (decl_index + 1u < decl_count) {
    return decls[decl_index + 1u].line_start;
  }
  return source_len;
}

static uint32_t decl_expression_end(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t decl_index
) {
  FnDecl decl = decls[decl_index];
  uint32_t function_end = decl_function_end(decls, decl_count, decl_index, source.len);
  if (decl.body_end <= decl.body_start || decl.body_end >= function_end) {
    return function_end;
  }
  uint32_t start = skip_expr_ws(source, decl.body_start, function_end);
  if (start >= function_end) {
    return function_end;
  }
  if (span_matches_keyword(source, start, function_end, "let")) {
    return function_end;
  }
  if (span_matches_keyword(source, start, function_end, "case")) {
    uint32_t of_at = find_case_of_at(source, start + 4u, decl.body_end);
    if (of_at < decl.body_end) {
      uint32_t after_of = skip_expr_ws(source, of_at + 2u, function_end);
      if (after_of < function_end && after_of >= decl.body_end) {
        return function_end;
      }
    }
  }
  {
    uint32_t physical_line_end = source_line_end(source, decl.line_start);
    physical_line_end = extend_lambda_truncated_line_end(
      source,
      decl.line_start,
      physical_line_end
    );
    if (decl.body_end < physical_line_end) {
      return physical_line_end;
    }
  }
  return decl.body_end;
}

static EvalValue lookup_eval_value(
  NameSpan name,
  NameSpan *params,
  EvalValue *values,
  uint32_t count
) {
  for (uint32_t i = 0u; i < count; i += 1u) {
    if (names_equal(name, params[i]) && values[i].ok) {
      return values[i];
    }
  }
  return missing_eval_value();
}

static EvalValue apply_eval_builtin_value(NameSpan name, EvalValue *args, uint32_t argc) {
  if (namespan_equals_literal(name, "id") && argc == 1u && args[0].ok) {
    return args[0];
  }
  if (namespan_equals_literal(name, "add") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value + args[1].int_value);
  }
  if (namespan_equals_literal(name, "sub") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value - args[1].int_value);
  }
  if (namespan_equals_literal(name, "mul") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value * args[1].int_value);
  }
  if (namespan_equals_literal(name, "div") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1]) &&
      args[1].int_value != 0) {
    return make_eval_int_value(args[0].int_value / args[1].int_value);
  }
  if (namespan_equals_literal(name, "mod") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1]) &&
      args[1].int_value != 0) {
    return make_eval_int_value(args[0].int_value % args[1].int_value);
  }
  if (namespan_equals_literal(name, "eq") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value == args[1].int_value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "ne") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value != args[1].int_value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "lt") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value < args[1].int_value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "le") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value <= args[1].int_value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "gt") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value > args[1].int_value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "ge") && argc == 2u &&
      eval_value_is_int(args[0]) && eval_value_is_int(args[1])) {
    return make_eval_int_value(args[0].int_value >= args[1].int_value ? 1 : 0);
  }
  if (namespan_equals_literal(name, "slice_new_u8") && argc == 1u &&
      eval_value_is_int(args[0]) && args[0].int_value >= 0) {
    return make_eval_slice_value((uint32_t) args[0].int_value);
  }
  if (namespan_equals_literal(name, "slice_get_u8") && argc == 2u &&
      args[0].ok && args[0].kind == EVAL_VALUE_SLICE &&
      eval_value_is_int(args[1]) && args[1].int_value >= 0) {
    uint32_t index = (uint32_t) args[1].int_value;
    if (index >= args[0].slice_len) {
      return make_eval_int_value(0);
    }
    return make_eval_int_value((int32_t) args[0].slice_bytes[index]);
  }
  if (namespan_equals_literal(name, "slice_set_u8") && argc == 3u &&
      args[0].ok && args[0].kind == EVAL_VALUE_SLICE &&
      eval_value_is_int(args[1]) && eval_value_is_int(args[2]) &&
      args[1].int_value >= 0 &&
      (uint32_t) args[1].int_value < args[0].slice_len &&
      args[2].int_value >= 0 && args[2].int_value <= 255) {
    EvalValue out = args[0];
    out.slice_bytes[(uint32_t) args[1].int_value] = (uint8_t) args[2].int_value;
    return out;
  }
  if (namespan_equals_literal(name, "str_to_slice") && argc == 1u &&
      args[0].ok && args[0].kind == EVAL_VALUE_SLICE) {
    return args[0];
  }
  return missing_eval_value();
}

static int is_simple_clause_block(
  Segment source,
  FnDecl decl,
  uint32_t function_end
) {
  uint32_t start = skip_expr_ws(source, decl.body_start, function_end);
  if (start >= function_end) {
    return 0;
  }
  if (span_matches_keyword(source, start, function_end, "let") ||
      span_matches_keyword(source, start, function_end, "if")) {
    return 0;
  }
  uint32_t line_end = source_line_end(source, start);
  if (line_end > function_end) {
    line_end = function_end;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  for (uint32_t at = start; at < line_end; at += 1u) {
    if (mem[at] == '=') {
      return 1;
    }
  }
  return 0;
}

static EvalValue eval_decl_by_name_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan name,
  EvalValue *args,
  uint32_t argc,
  uint32_t depth
);

static EvalValue eval_expr_value_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  NameSpan *params,
  EvalValue *param_values,
  uint32_t param_count,
  uint32_t depth
);

static EvalValue eval_let_expr_value_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  NameSpan *params,
  EvalValue *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  if (depth > 32u) {
    return missing_eval_value();
  }
  uint32_t cursor = skip_expr_ws(source, start, end);
  if (!span_matches_keyword(source, cursor, end, "let")) {
    return missing_eval_value();
  }
  cursor += 3u;
  NameSpan local_names[MAX_EVAL_LOCALS];
  EvalValue local_values[MAX_EVAL_LOCALS];
  uint32_t local_count = 0u;
  for (uint32_t i = 0u; i < param_count && i < MAX_EVAL_LOCALS; i += 1u) {
    local_names[local_count] = params[i];
    local_values[local_count] = param_values[i];
    local_count += 1u;
  }
  while (cursor < end) {
    cursor = skip_expr_ws(source, cursor, end);
    if (cursor >= end) {
      break;
    }
    if (span_matches_keyword(source, cursor, end, "in")) {
      cursor += 2u;
      return eval_expr_value_extended(
        source,
        decls,
        decl_count,
        cursor,
        end,
        local_names,
        local_values,
        local_count,
        depth + 1u
      );
    }
    if (local_count >= MAX_EVAL_LOCALS) {
      return missing_eval_value();
    }
    uint32_t name_next = cursor;
    NameSpan name = parse_simple_name_token(source, cursor, end, &name_next);
    if (!name.ok) {
      return missing_eval_value();
    }
    uint32_t line_end = source_line_end(source, cursor);
    if (line_end > end) {
      line_end = end;
    }
    uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
    uint32_t eq_at = name_next;
    while (eq_at < line_end && mem[eq_at] != '=') {
      eq_at += 1u;
    }
    if (eq_at >= line_end) {
      return missing_eval_value();
    }
    EvalValue value = eval_expr_value_extended(
      source,
      decls,
      decl_count,
      eq_at + 1u,
      line_end,
      local_names,
      local_values,
      local_count,
      depth + 1u
    );
    if (!value.ok) {
      return missing_eval_value();
    }
    local_names[local_count] = name;
    local_values[local_count] = value;
    local_count += 1u;
    cursor = source_next_line_start(source, line_end);
  }
  return missing_eval_value();
}

static EvalValue eval_clause_decl_value_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  FnDecl decl,
  uint32_t function_end,
  uint32_t argc,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  NameSpan *params,
  EvalValue *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  if (argc > MAX_EVAL_ARGS || depth > 32u) {
    return missing_eval_value();
  }
  EvalValue arg_cache[MAX_EVAL_ARGS];
  int arg_cached[MAX_EVAL_ARGS];
  for (uint32_t i = 0u; i < argc; i += 1u) {
    arg_cached[i] = 0;
  }
  uint32_t line_start = decl.body_start;
  while (line_start < function_end) {
    uint32_t line_end = source_line_end(source, line_start);
    if (line_end > function_end) {
      line_end = function_end;
    }
    uint32_t next_line = source_next_line_start(source, line_end);
    uint32_t cursor = source_skip_line_ws(source, line_start, line_end);
    uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
    if (cursor >= line_end || (mem[cursor] == '-' && cursor + 1u < line_end && mem[cursor + 1u] == '-')) {
      line_start = next_line;
      continue;
    }
    uint32_t eq_at = find_top_level_assignment_eq(source, cursor, line_end);
    if (eq_at >= line_end) {
      line_start = next_line;
      continue;
    }
    uint32_t pat_cursor = cursor;
    int matched = 1;
    for (uint32_t arg_index = 0u; arg_index < argc; arg_index += 1u) {
      pat_cursor = skip_expr_ws(source, pat_cursor, eq_at);
      if (pat_cursor >= eq_at) {
        matched = 0;
        break;
      }
      if (mem[pat_cursor] == '_') {
        pat_cursor += 1u;
        continue;
      }
      int32_t int_value = 0;
      uint32_t next = pat_cursor;
      if (parse_signed_int_literal(source, pat_cursor, eq_at, &int_value, &next)) {
        if (!arg_cached[arg_index]) {
          arg_cache[arg_index] = eval_expr_value_extended(
            source,
            decls,
            decl_count,
            arg_starts[arg_index],
            arg_ends[arg_index],
            params,
            param_values,
            param_count,
            depth + 1u
          );
          arg_cached[arg_index] = 1;
        }
        if (!eval_value_is_int(arg_cache[arg_index]) || arg_cache[arg_index].int_value != int_value) {
          matched = 0;
          break;
        }
        pat_cursor = next;
        continue;
      }
      {
        NameSpan pattern = parse_simple_name_token(source, pat_cursor, eq_at, &next);
        if (!pattern.ok) {
          matched = 0;
          break;
        }
        if (namespan_equals_literal(pattern, "true") || namespan_equals_literal(pattern, "True") ||
            namespan_equals_literal(pattern, "false") || namespan_equals_literal(pattern, "False")) {
          int32_t expected = (namespan_equals_literal(pattern, "true") || namespan_equals_literal(pattern, "True")) ? 1 : 0;
          if (!arg_cached[arg_index]) {
            arg_cache[arg_index] = eval_expr_value_extended(
              source,
              decls,
              decl_count,
              arg_starts[arg_index],
              arg_ends[arg_index],
              params,
              param_values,
              param_count,
              depth + 1u
            );
            arg_cached[arg_index] = 1;
          }
          if (!eval_value_is_int(arg_cache[arg_index]) || arg_cache[arg_index].int_value != expected) {
            matched = 0;
            break;
          }
          pat_cursor = next;
          continue;
        }
        matched = 0;
        break;
      }
    }
    if (matched) {
      return eval_expr_value_extended(
        source,
        decls,
        decl_count,
        eq_at + 1u,
        line_end,
        params,
        param_values,
        param_count,
        depth + 1u
      );
    }
    line_start = next_line;
  }
  return missing_eval_value();
}

static EvalValue eval_decl_body_value_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t decl_index,
  NameSpan *params,
  EvalValue *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  FnDecl decl = decls[decl_index];
  uint32_t function_end = decl_function_end(decls, decl_count, decl_index, source.len);
  if (is_simple_clause_block(source, decl, function_end)) {
    return missing_eval_value();
  }
  uint32_t expr_end = decl_expression_end(source, decls, decl_count, decl_index);
  return eval_expr_value_extended(
    source,
    decls,
    decl_count,
    decl.body_start,
    expr_end,
    params,
    param_values,
    param_count,
    depth + 1u
  );
}

static EvalValue eval_decl_by_name_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan name,
  EvalValue *args,
  uint32_t argc,
  uint32_t depth
) {
  if (depth > 32u) {
    return missing_eval_value();
  }
  int decl_index = find_decl_index_by_name(decls, decl_count, name);
  if (decl_index < 0) {
    return missing_eval_value();
  }
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = collect_decl_params(source, decls[(uint32_t) decl_index], params, MAX_EVAL_ARGS);
  if (param_count != argc) {
    return missing_eval_value();
  }
  return eval_decl_body_value_extended(
    source,
    decls,
    decl_count,
    (uint32_t) decl_index,
    params,
    args,
    argc,
    depth + 1u
  );
}

static EvalValue eval_atom_value_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  NameSpan *params,
  EvalValue *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  start = skip_expr_ws(source, start, end);
  end = trim_expr_end(source, start, end);
  if (start >= end) {
    return missing_eval_value();
  }
  if (span_is_wrapped_parens(source, start, end)) {
    return eval_expr_value_extended(
      source,
      decls,
      decl_count,
      start + 1u,
      end - 1u,
      params,
      param_values,
      param_count,
      depth + 1u
    );
  }
  {
    int32_t int_value = 0;
    uint32_t next = start;
    if (parse_signed_int_literal(source, start, end, &int_value, &next) && next == end) {
      return make_eval_int_value(int_value);
    }
  }
  {
    uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
    if (mem[start] == '"' || (start + 1u < end && mem[start] == '\\' && mem[start + 1u] == '"')) {
      int escaped_quotes = mem[start] == '\\' ? 1 : 0;
      uint32_t string_end = parse_string_literal_end(source, start, end);
      if (string_end == end && end > start) {
        EvalValue out = make_eval_slice_value(0u);
        uint32_t cursor = start + (escaped_quotes ? 2u : 1u);
        uint32_t len = 0u;
        while (cursor < end && len < MAX_EVAL_SLICE_BYTES) {
          if (escaped_quotes && cursor + 1u < end &&
              mem[cursor] == '\\' && mem[cursor + 1u] == '"') {
            break;
          }
          if (!escaped_quotes && cursor + 1u == end) {
            break;
          }
          if (mem[cursor] == '\\' && cursor + 1u < end - 1u) {
            cursor += 1u;
          }
          out.slice_bytes[len++] = mem[cursor];
          cursor += 1u;
        }
        out.slice_len = len;
        out.ok = 1;
        out.kind = EVAL_VALUE_SLICE;
        return out;
      }
    }
  }
  {
    uint32_t next = start;
    NameSpan name = parse_simple_name_token(source, start, end, &next);
    if (name.ok && next == end) {
      if (namespan_equals_literal(name, "true") || namespan_equals_literal(name, "True")) {
        return make_eval_int_value(1);
      }
      if (namespan_equals_literal(name, "false") || namespan_equals_literal(name, "False")) {
        return make_eval_int_value(0);
      }
      {
        EvalValue local = lookup_eval_value(name, params, param_values, param_count);
        if (local.ok) {
          return local;
        }
      }
      return eval_decl_by_name_extended(source, decls, decl_count, name, NULL, 0u, depth + 1u);
    }
  }
  return missing_eval_value();
}

static EvalValue eval_if_expr_value_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  NameSpan *params,
  EvalValue *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  if (!span_matches_keyword(source, start, end, "if")) {
    return missing_eval_value();
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t cursor = start + 2u;
  cursor = skip_expr_ws(source, cursor, end);
  uint32_t then_at = cursor;
  uint32_t nesting = 0u;
  int in_string = 0;
  int escaped = 0;
  while (then_at < end) {
    uint8_t c = mem[then_at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      then_at += 1u;
      continue;
    }
    if (c == '"') {
      in_string = 1;
      then_at += 1u;
      continue;
    }
    if (c == '(') {
      nesting += 1u;
    } else if (c == ')' && nesting > 0u) {
      nesting -= 1u;
    } else if (nesting == 0u && span_matches_keyword(source, then_at, end, "then")) {
      break;
    }
    then_at += 1u;
  }
  if (then_at >= end) {
    return missing_eval_value();
  }
  EvalValue cond = eval_expr_value_extended(
    source, decls, decl_count, cursor, then_at, params, param_values, param_count, depth + 1u
  );
  if (!eval_value_is_int(cond)) {
    return missing_eval_value();
  }
  uint32_t else_at = then_at + 4u;
  else_at = skip_expr_ws(source, else_at, end);
  uint32_t branch_split = else_at;
  nesting = 0u;
  in_string = 0;
  escaped = 0;
  while (branch_split < end) {
    uint8_t c = mem[branch_split];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      branch_split += 1u;
      continue;
    }
    if (c == '"') {
      in_string = 1;
      branch_split += 1u;
      continue;
    }
    if (c == '(') {
      nesting += 1u;
    } else if (c == ')' && nesting > 0u) {
      nesting -= 1u;
    } else if (nesting == 0u && span_matches_keyword(source, branch_split, end, "else")) {
      break;
    }
    branch_split += 1u;
  }
  if (branch_split >= end) {
    return missing_eval_value();
  }
  if (cond.int_value != 0) {
    return eval_expr_value_extended(
      source, decls, decl_count, else_at, branch_split, params, param_values, param_count, depth + 1u
    );
  }
  return eval_expr_value_extended(
    source, decls, decl_count, branch_split + 4u, end, params, param_values, param_count, depth + 1u
  );
}

static EvalValue eval_expr_value_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  NameSpan *params,
  EvalValue *param_values,
  uint32_t param_count,
  uint32_t depth
) {
  if (depth > 32u) {
    return missing_eval_value();
  }
  start = skip_expr_ws(source, start, end);
  end = trim_expr_end(source, start, end);
  if (start >= end) {
    return missing_eval_value();
  }
  if (span_matches_keyword(source, start, end, "let")) {
    EvalValue let_value = eval_let_expr_value_extended(
      source, decls, decl_count, start, end, params, param_values, param_count, depth + 1u
    );
    if (let_value.ok) {
      return let_value;
    }
  }
  if (span_matches_keyword(source, start, end, "if")) {
    EvalValue if_value = eval_if_expr_value_extended(
      source, decls, decl_count, start, end, params, param_values, param_count, depth + 1u
    );
    if (if_value.ok) {
      return if_value;
    }
  }
  uint32_t head_start = skip_expr_ws(source, start, end);
  uint32_t head_end = parse_expr_atom_end(source, head_start, end);
  if (head_end <= head_start) {
    return missing_eval_value();
  }
  uint32_t arg_starts[MAX_EVAL_ARGS];
  uint32_t arg_ends[MAX_EVAL_ARGS];
  uint32_t argc = 0u;
  uint32_t cursor = head_end;
  while (1) {
    cursor = skip_expr_ws(source, cursor, end);
    if (cursor >= end) {
      break;
    }
    if (argc >= MAX_EVAL_ARGS) {
      return missing_eval_value();
    }
    uint32_t atom_end = parse_expr_atom_end(source, cursor, end);
    if (atom_end <= cursor) {
      return missing_eval_value();
    }
    arg_starts[argc] = cursor;
    arg_ends[argc] = atom_end;
    argc += 1u;
    cursor = atom_end;
  }
  if (argc == 0u) {
    return eval_atom_value_extended(
      source, decls, decl_count, head_start, head_end, params, param_values, param_count, depth + 1u
    );
  }
  {
    uint32_t next = head_start;
    NameSpan head = parse_simple_name_token(source, head_start, head_end, &next);
    if (!head.ok || next != head_end) {
      return missing_eval_value();
    }
    EvalValue builtin_args[MAX_EVAL_ARGS];
    int builtin_ready = 1;
    for (uint32_t i = 0u; i < argc; i += 1u) {
      builtin_args[i] = eval_expr_value_extended(
        source,
        decls,
        decl_count,
        arg_starts[i],
        arg_ends[i],
        params,
        param_values,
        param_count,
        depth + 1u
      );
      if (!builtin_args[i].ok) {
        builtin_ready = 0;
      }
    }
    if (builtin_ready) {
      EvalValue builtin = apply_eval_builtin_value(head, builtin_args, argc);
      if (builtin.ok) {
        return builtin;
      }
    }
    {
      int decl_index = find_decl_index_by_name(decls, decl_count, head);
      if (decl_index >= 0) {
        uint32_t function_end = decl_function_end(decls, decl_count, (uint32_t) decl_index, source.len);
        if (is_simple_clause_block(source, decls[(uint32_t) decl_index], function_end)) {
          return eval_clause_decl_value_extended(
            source,
            decls,
            decl_count,
            decls[(uint32_t) decl_index],
            function_end,
            argc,
            arg_starts,
            arg_ends,
            params,
            param_values,
            param_count,
            depth + 1u
          );
        }
      }
    }
    EvalValue args[MAX_EVAL_ARGS];
    for (uint32_t i = 0u; i < argc; i += 1u) {
      args[i] = eval_expr_value_extended(
        source,
        decls,
        decl_count,
        arg_starts[i],
        arg_ends[i],
        params,
        param_values,
        param_count,
        depth + 1u
      );
      if (!args[i].ok) {
        return missing_eval_value();
      }
    }
    return eval_decl_by_name_extended(source, decls, decl_count, head, args, argc, depth + 1u);
  }
}

static EvalConst eval_root_extended(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan root
) {
  return eval_const_from_value(
    eval_decl_by_name_extended(source, decls, decl_count, root, NULL, 0u, 0u)
  );
}

#define MAX_RAW_EMIT_EXPR_BINDINGS 64u
#define MAX_RAW_EMIT_INLINE_DEPTH 8u
#define MAX_RAW_EMIT_CTOR_BINDINGS 64u
#define MISSING_CTOR_BINDING 0xffffffffu

static void init_ctor_binding_array(uint32_t *bindings, uint32_t count) {
  if (!bindings) {
    return;
  }
  for (uint32_t i = 0u; i < count; i += 1u) {
    bindings[i] = MISSING_CTOR_BINDING;
  }
}

static uint32_t encode_var_s32_bytes(int32_t value, uint8_t *out);
static int find_let_binding_split(
  Segment source,
  uint32_t start,
  uint32_t end,
  uint32_t line_end,
  uint32_t *value_end,
  uint32_t *next_cursor,
  int *found_in
);

typedef struct {
  NameSpan ctor_name;
  uint32_t expr_start;
  uint32_t expr_end;
  uint32_t arg_starts[MAX_EVAL_ARGS];
  uint32_t arg_ends[MAX_EVAL_ARGS];
  uint32_t arg_ctor_bindings[MAX_EVAL_ARGS];
  uint32_t arg_count;
} RawCtorBinding;

typedef struct {
  NameSpan names[MAX_EVAL_LOCALS];
  uint32_t indices[MAX_EVAL_LOCALS];
  uint32_t count;
  uint32_t next_local_index;
  int *function_index_by_decl;
  NameSpan expr_names[MAX_RAW_EMIT_EXPR_BINDINGS];
  uint32_t expr_starts[MAX_RAW_EMIT_EXPR_BINDINGS];
  uint32_t expr_ends[MAX_RAW_EMIT_EXPR_BINDINGS];
  uint32_t expr_ctor_bindings[MAX_RAW_EMIT_EXPR_BINDINGS];
  uint32_t expr_count;
  RawCtorBinding ctor_bindings[MAX_RAW_EMIT_CTOR_BINDINGS];
  uint32_t ctor_binding_count;
} RawEmitEnv;

static int raw_emit_expr_to_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
);

static int raw_capture_constructor_binding(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint32_t start,
  uint32_t end,
  uint32_t *binding_index_out,
  uint32_t depth
);

static int raw_emit_lookup_expr_binding(
  NameSpan name,
  RawEmitEnv *env,
  uint32_t *start_out,
  uint32_t *end_out,
  uint32_t *ctor_binding_out
);

static int raw_emit_find_decl(FnDecl *decls, uint32_t decl_count, NameSpan name, uint32_t *index_out);

static int raw_emit_bind_expr_name(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  NameSpan name,
  uint32_t expr_start,
  uint32_t expr_end
);

static int raw_resolve_bound_expr_span(
  Segment source,
  RawEmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *resolved_start_out,
  uint32_t *resolved_end_out,
  uint32_t depth
);

static int raw_parse_apply_span(
  Segment source,
  uint32_t start,
  uint32_t end,
  NameSpan *head_out,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t max_args,
  uint32_t *argc_out
);

static int raw_emit_apply_named_builtin(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan head,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
);

static int raw_emit_inline_named_decl(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan head,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
);

static void raw_normalize_ctor_binding(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint32_t binding_index,
  uint32_t expr_start,
  uint32_t expr_end,
  uint32_t depth
);

static int raw_emit_append_byte(uint8_t *out, uint32_t *cursor, uint32_t limit, uint8_t byte) {
  if (*cursor >= limit) {
    return 0;
  }
  out[*cursor] = byte;
  *cursor += 1u;
  return 1;
}

static int raw_emit_append_bytes(
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit,
  const uint8_t *bytes,
  uint32_t len
) {
  if (*cursor > limit || len > limit - *cursor) {
    return 0;
  }
  for (uint32_t i = 0u; i < len; i += 1u) {
    out[*cursor + i] = bytes[i];
  }
  *cursor += len;
  return 1;
}

static int raw_emit_append_var_u32(uint8_t *out, uint32_t *cursor, uint32_t limit, uint32_t value) {
  uint8_t bytes[5];
  uint32_t len = 0u;
  uint32_t n = value;
  do {
    uint8_t byte = (uint8_t) (n & 0x7fu);
    n >>= 7;
    if (n != 0u) {
      byte |= 0x80u;
    }
    bytes[len++] = byte;
  } while (n != 0u && len < 5u);
  return raw_emit_append_bytes(out, cursor, limit, bytes, len);
}

static int raw_emit_append_var_s32(uint8_t *out, uint32_t *cursor, uint32_t limit, int32_t value) {
  uint8_t bytes[5];
  uint32_t len = encode_var_s32_bytes(value, bytes);
  return raw_emit_append_bytes(out, cursor, limit, bytes, len);
}

static int raw_emit_reserve_temp_local(RawEmitEnv *env, uint32_t *index_out) {
  if (!env || !index_out || env->next_local_index >= 0xffffffffu) {
    return 0;
  }
  *index_out = env->next_local_index;
  env->next_local_index += 1u;
  return 1;
}

static int raw_emit_if_branch_to_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t branch_start,
  uint32_t branch_end,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint32_t zero_local,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  branch_start = skip_expr_ws(source, branch_start, branch_end);
  branch_end = trim_expr_end(source, branch_start, branch_end);
  if (branch_start < branch_end &&
      span_is_wrapped_parens(source, branch_start, branch_end)) {
    branch_start += 1u;
    branch_end -= 1u;
  }
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = 0u;
  uint32_t body_start = 0u;
  if (!parse_lambda_expr(
        source,
        branch_start,
        branch_end,
        params,
        MAX_EVAL_ARGS,
        &param_count,
        &body_start
      )) {
    return raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      branch_start,
      branch_end,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
  }
  if (param_count != 1u) {
    return 0;
  }
  uint32_t saved_count = env->count;
  env->names[env->count] = params[0];
  env->indices[env->count] = zero_local;
  env->count += 1u;
  int ok = raw_emit_expr_to_wasm(
    source,
    decls,
    decl_count,
    body_start,
    branch_end,
    env,
    inline_stack,
    inline_depth + 1u,
    out,
    cursor,
    limit
  );
  env->count = saved_count;
  return ok;
}

static int raw_emit_bound_apply_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t start,
  uint32_t end,
  uint32_t *extra_arg_starts,
  uint32_t *extra_arg_ends,
  uint32_t extra_argc,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit,
  uint32_t inline_depth
);

static int raw_emit_lambda_apply_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t start,
  uint32_t end,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit,
  uint32_t inline_depth
) {
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = 0u;
  uint32_t body_start = 0u;
  if (!parse_lambda_expr(
        source,
        start,
        end,
        params,
        MAX_EVAL_ARGS,
        &param_count,
        &body_start)) {
    return 0;
  }
  if (param_count == 0u || argc < param_count ||
      env->expr_count + param_count > MAX_RAW_EMIT_EXPR_BINDINGS) {
    return 0;
  }
  uint32_t saved_expr_count = env->expr_count;
  uint32_t saved_ctor_binding_count = env->ctor_binding_count;
  for (uint32_t i = 0u; i < param_count; i += 1u) {
    uint32_t resolved_start = arg_starts[i];
    uint32_t resolved_end = arg_ends[i];
    raw_resolve_bound_expr_span(
      source,
      env,
      arg_starts[i],
      arg_ends[i],
      &resolved_start,
      &resolved_end,
      0u
    );
    if (!raw_emit_bind_expr_name(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth + 1u,
          params[i],
          resolved_start,
          resolved_end)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
  }
  int ok = 0;
  if (argc == param_count) {
    ok = raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      body_start,
      end,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
  } else {
    ok = raw_emit_bound_apply_wasm(
      source,
      decls,
      decl_count,
      env,
      inline_stack,
      body_start,
      end,
      arg_starts + param_count,
      arg_ends + param_count,
      argc - param_count,
      out,
      cursor,
      limit,
      inline_depth + 1u
    );
  }
  env->expr_count = saved_expr_count;
  env->ctor_binding_count = saved_ctor_binding_count;
  return ok;
}

static int raw_emit_bound_apply_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t start,
  uint32_t end,
  uint32_t *extra_arg_starts,
  uint32_t *extra_arg_ends,
  uint32_t extra_argc,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit,
  uint32_t inline_depth
) {
  if (inline_depth > MAX_RAW_EMIT_INLINE_DEPTH) {
    return 0;
  }
  if (!raw_resolve_bound_expr_span(source, env, start, end, &start, &end, 0u)) {
    return 0;
  }
  if (raw_emit_lambda_apply_wasm(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        start,
        end,
        extra_arg_starts,
        extra_arg_ends,
        extra_argc,
        out,
        cursor,
        limit,
        inline_depth + 1u)) {
    return 1;
  }
  if (span_is_wrapped_parens(source, start, end)) {
    return raw_emit_bound_apply_wasm(
      source,
      decls,
      decl_count,
      env,
      inline_stack,
      start + 1u,
      end - 1u,
      extra_arg_starts,
      extra_arg_ends,
      extra_argc,
      out,
      cursor,
      limit,
      inline_depth + 1u
    );
  }
  {
    uint32_t next = start;
    NameSpan simple = parse_simple_name_token(source, start, end, &next);
    if (simple.ok && next == end) {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      if (raw_emit_lookup_expr_binding(simple, env, &bound_start, &bound_end, NULL)) {
        return raw_emit_bound_apply_wasm(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          bound_start,
          bound_end,
          extra_arg_starts,
          extra_arg_ends,
          extra_argc,
          out,
          cursor,
          limit,
          inline_depth + 1u
        );
      }
      {
        uint32_t decl_index = 0u;
        if (raw_emit_find_decl(decls, decl_count, simple, &decl_index) &&
            decl_param_count(source, decls[decl_index]) == 0u) {
          FnDecl decl = decls[decl_index];
          uint32_t expr_end = decl.body_end > decl.body_start
            ? decl.body_end
            : decl_expression_end(source, decls, decl_count, decl_index);
          return raw_emit_bound_apply_wasm(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            decl.body_start,
            expr_end,
            extra_arg_starts,
            extra_arg_ends,
            extra_argc,
            out,
            cursor,
            limit,
            inline_depth + 1u
          );
        }
      }
    }
  }
  {
    NameSpan head = missing_name_span();
    uint32_t arg_starts[MAX_EVAL_ARGS] = {0};
    uint32_t arg_ends[MAX_EVAL_ARGS] = {0};
    uint32_t argc = 0u;
    if (!raw_parse_apply_span(
          source,
          start,
          end,
          &head,
          arg_starts,
          arg_ends,
          MAX_EVAL_ARGS,
          &argc)) {
      return 0;
    }
    if (argc + extra_argc > MAX_EVAL_ARGS) {
      return 0;
    }
    for (uint32_t i = 0u; i < extra_argc; i += 1u) {
      arg_starts[argc + i] = extra_arg_starts[i];
      arg_ends[argc + i] = extra_arg_ends[i];
    }
    if (raw_emit_apply_named_builtin(
          source,
          decls,
          decl_count,
          head,
          arg_starts,
          arg_ends,
          argc + extra_argc,
          env,
          inline_stack,
          inline_depth + 1u,
          out,
          cursor,
          limit)) {
      return 1;
    }
    if (raw_emit_inline_named_decl(
          source,
          decls,
          decl_count,
          head,
          arg_starts,
          arg_ends,
          argc + extra_argc,
          env,
          inline_stack,
          inline_depth + 1u,
          out,
          cursor,
          limit)) {
      return 1;
    }
    {
      uint32_t decl_index = 0u;
      if (raw_emit_find_decl(decls, decl_count, head, &decl_index) &&
          env != NULL &&
          env->function_index_by_decl != NULL &&
          env->function_index_by_decl[decl_index] >= 0 &&
          decl_param_count(source, decls[decl_index]) == argc + extra_argc) {
        for (uint32_t i = 0u; i < argc + extra_argc; i += 1u) {
          if (!raw_emit_expr_to_wasm(
                source,
                decls,
                decl_count,
                arg_starts[i],
                arg_ends[i],
                env,
                inline_stack,
                inline_depth + 1u,
                out,
                cursor,
                limit
              )) {
            return 0;
          }
        }
        return raw_emit_append_byte(out, cursor, limit, 0x10u) &&
          raw_emit_append_var_u32(out, cursor, limit, (uint32_t) env->function_index_by_decl[decl_index]);
      }
    }
  }
  return 0;
}

static int raw_emit_lookup_local(NameSpan name, RawEmitEnv *env, uint32_t *index_out) {
  if (!env) {
    return 0;
  }
  for (uint32_t i = env->count; i > 0u; i -= 1u) {
    if (names_equal(name, env->names[i - 1u])) {
      *index_out = env->indices[i - 1u];
      return 1;
    }
  }
  return 0;
}

static int raw_emit_lookup_expr_binding(
  NameSpan name,
  RawEmitEnv *env,
  uint32_t *start_out,
  uint32_t *end_out,
  uint32_t *ctor_binding_out
) {
  if (!env) {
    return 0;
  }
  for (uint32_t i = env->expr_count; i > 0u; i -= 1u) {
    if (names_equal(name, env->expr_names[i - 1u])) {
      *start_out = env->expr_starts[i - 1u];
      *end_out = env->expr_ends[i - 1u];
      if (ctor_binding_out) {
        *ctor_binding_out = env->expr_ctor_bindings[i - 1u];
      }
      return 1;
    }
  }
  return 0;
}

static int raw_emit_bind_expr_name_with_ctor(
  RawEmitEnv *env,
  NameSpan name,
  uint32_t expr_start,
  uint32_t expr_end,
  uint32_t ctor_binding
) {
  if (!env || env->expr_count >= MAX_RAW_EMIT_EXPR_BINDINGS) {
    return 0;
  }
  env->expr_names[env->expr_count] = name;
  env->expr_starts[env->expr_count] = expr_start;
  env->expr_ends[env->expr_count] = expr_end;
  env->expr_ctor_bindings[env->expr_count] = ctor_binding;
  env->expr_count += 1u;
  return 1;
}

static int raw_emit_find_decl(FnDecl *decls, uint32_t decl_count, NameSpan name, uint32_t *index_out) {
  int decl_index = find_decl_index_by_name(decls, decl_count, name);
  if (decl_index < 0) {
    return 0;
  }
  *index_out = (uint32_t) decl_index;
  return 1;
}

static int raw_emit_inline_stack_has(NameSpan *inline_stack, uint32_t inline_depth, NameSpan name) {
  for (uint32_t i = 0u; i < inline_depth; i += 1u) {
    if (names_equal(inline_stack[i], name)) {
      return 1;
    }
  }
  return 0;
}

static int raw_emit_bind_expr_name(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  NameSpan name,
  uint32_t expr_start,
  uint32_t expr_end
) {
  if (span_is_exact_simple_name(source, expr_start, expr_end, name)) {
    return 1;
  }
  uint32_t ctor_binding = MISSING_CTOR_BINDING;
  if (
    env &&
    raw_capture_constructor_binding(
      source,
      decls,
      decl_count,
      env,
      inline_stack,
      inline_depth + 1u,
      expr_start,
      expr_end,
      &ctor_binding,
      0u
    )
  ) {
    RawCtorBinding *binding = &env->ctor_bindings[ctor_binding];
    return raw_emit_bind_expr_name_with_ctor(
      env,
      name,
      binding->expr_start,
      binding->expr_end,
      ctor_binding
    );
  }
  return raw_emit_bind_expr_name_with_ctor(
    env,
    name,
    expr_start,
    expr_end,
    MISSING_CTOR_BINDING
  );
}

static int raw_resolve_bound_expr_span(
  Segment source,
  RawEmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *out_start,
  uint32_t *out_end,
  uint32_t depth
) {
  if (depth > 16u) {
    return 0;
  }
  start = skip_expr_ws(source, start, end);
  end = trim_expr_end(source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_is_wrapped_parens(source, start, end)) {
    return raw_resolve_bound_expr_span(source, env, start + 1u, end - 1u, out_start, out_end, depth + 1u);
  }
  {
    uint32_t next = start;
    NameSpan name = parse_simple_name_token(source, start, end, &next);
    if (name.ok && next == end) {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      if (raw_emit_lookup_expr_binding(name, env, &bound_start, &bound_end, NULL)) {
        return raw_resolve_bound_expr_span(source, env, bound_start, bound_end, out_start, out_end, depth + 1u);
      }
    }
  }
  *out_start = start;
  *out_end = end;
  return 1;
}

static int raw_parse_apply_span(
  Segment source,
  uint32_t start,
  uint32_t end,
  NameSpan *head_out,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t max_args,
  uint32_t *argc_out
) {
  uint32_t head_start = skip_expr_ws(source, start, end);
  uint32_t head_end = parse_expr_atom_end(source, head_start, end);
  if (head_end <= head_start) {
    return 0;
  }
  uint32_t next = head_start;
  NameSpan head = parse_simple_name_token(source, head_start, head_end, &next);
  if (!head.ok || next != head_end) {
    return 0;
  }
  uint32_t argc = 0u;
  uint32_t cursor = skip_expr_ws(source, head_end, end);
  while (cursor < end) {
    if (argc >= max_args) {
      return 0;
    }
    uint32_t atom_end = parse_expr_atom_end(source, cursor, end);
    if (atom_end <= cursor) {
      return 0;
    }
    arg_starts[argc] = cursor;
    arg_ends[argc] = atom_end;
    argc += 1u;
    cursor = skip_expr_ws(source, atom_end, end);
  }
  *head_out = head;
  *argc_out = argc;
  return 1;
}

static int raw_resolve_direct_constructor_target(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint32_t start,
  uint32_t end,
  NameSpan *ctor_name_out,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t *arg_ctor_bindings,
  uint32_t max_args,
  uint32_t *argc_out,
  uint32_t depth
) {
  if (depth > MAX_RAW_EMIT_INLINE_DEPTH) {
    return 0;
  }
  {
    uint32_t simple_start = skip_expr_ws(source, start, end);
    uint32_t simple_end = trim_expr_end(source, simple_start, end);
    if (span_is_wrapped_parens(source, simple_start, simple_end)) {
      simple_start += 1u;
      simple_end -= 1u;
    }
    if (simple_start < simple_end) {
      uint32_t next = simple_start;
      NameSpan simple = parse_simple_name_token(source, simple_start, simple_end, &next);
      if (simple.ok && next == simple_end) {
        uint32_t bound_start = 0u;
        uint32_t bound_end = 0u;
        uint32_t ctor_binding = MISSING_CTOR_BINDING;
        if (raw_emit_lookup_expr_binding(simple, env, &bound_start, &bound_end, &ctor_binding) &&
            ctor_binding != MISSING_CTOR_BINDING &&
            ctor_binding < env->ctor_binding_count) {
          RawCtorBinding *binding = &env->ctor_bindings[ctor_binding];
          if (binding->arg_count > max_args) {
            return 0;
          }
          *ctor_name_out = binding->ctor_name;
          *argc_out = binding->arg_count;
          for (uint32_t i = 0u; i < binding->arg_count; i += 1u) {
            arg_starts[i] = binding->arg_starts[i];
            arg_ends[i] = binding->arg_ends[i];
            if (arg_ctor_bindings) {
              arg_ctor_bindings[i] = binding->arg_ctor_bindings[i];
            }
          }
          return 1;
        }
      }
    }
  }
  if (!raw_resolve_bound_expr_span(source, env, start, end, &start, &end, 0u)) {
    return 0;
  }
  if (span_matches_keyword(source, start, end, "case")) {
    uint32_t of_at = find_case_of_at(source, start + 4u, end);
    if (of_at >= end) {
      return 0;
    }
    uint32_t target_start = skip_expr_ws(source, start + 4u, of_at);
    uint32_t target_end = trim_expr_end(source, target_start, of_at);
    if (target_start >= target_end) {
      return 0;
    }
    uint32_t line_start = skip_expr_ws(source, of_at + 2u, end);
    while (line_start < end) {
      uint32_t line_end = source_line_end(source, line_start);
      if (line_end > end) {
        line_end = end;
      }
      uint32_t next_line = source_next_line_start(source, line_end);
      uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
      if (line_start >= line_end ||
          (mem[line_start] == '-' && line_start + 1u < line_end && mem[line_start + 1u] == '-')) {
        line_start = skip_expr_ws(source, next_line, end);
        continue;
      }
      uint32_t arrow_at = find_case_arm_arrow(source, line_start, line_end);
      if (arrow_at >= line_end) {
        return 0;
      }
      uint32_t saved_expr_count = env->expr_count;
      uint32_t saved_ctor_binding_count = env->ctor_binding_count;
      uint32_t pat_start = line_start;
      uint32_t first_end = parse_expr_atom_end(source, pat_start, arrow_at);
      if (first_end <= pat_start) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      uint32_t next = pat_start;
      NameSpan first = parse_simple_name_token(source, pat_start, first_end, &next);
      int matched = 0;
      if (first.ok && next == first_end && namespan_starts_with_upper(source, first)) {
        NameSpan target_ctor = missing_name_span();
        uint32_t target_arg_starts[MAX_EVAL_ARGS] = {0};
        uint32_t target_arg_ends[MAX_EVAL_ARGS] = {0};
        uint32_t target_arg_ctor_bindings[MAX_EVAL_ARGS];
        init_ctor_binding_array(target_arg_ctor_bindings, MAX_EVAL_ARGS);
        uint32_t target_arg_count = 0u;
        if (!raw_resolve_direct_constructor_target(
              source,
              decls,
              decl_count,
              env,
              inline_stack,
              inline_depth + 1u,
              target_start,
              target_end,
              &target_ctor,
              target_arg_starts,
              target_arg_ends,
              target_arg_ctor_bindings,
              MAX_EVAL_ARGS,
              &target_arg_count,
              depth + 1u)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        if (!names_equal(first, target_ctor)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          line_start = skip_expr_ws(source, next_line, end);
          continue;
        }
        uint32_t pattern_cursor = skip_expr_ws(source, first_end, arrow_at);
        uint32_t arg_index = 0u;
        while (pattern_cursor < arrow_at) {
          if (arg_index >= target_arg_count) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          uint32_t pat_end = parse_expr_atom_end(source, pattern_cursor, arrow_at);
          if (pat_end <= pattern_cursor) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          if (!(pat_end == pattern_cursor + 1u && mem[pattern_cursor] == '_')) {
            uint32_t pat_next = pattern_cursor;
            NameSpan pat_name = parse_simple_name_token(source, pattern_cursor, pat_end, &pat_next);
            int bind_ok = 0;
            if (pat_name.ok && pat_next == pat_end) {
              if (target_arg_ctor_bindings[arg_index] != MISSING_CTOR_BINDING) {
                bind_ok = raw_emit_bind_expr_name_with_ctor(
                  env,
                  pat_name,
                  target_arg_starts[arg_index],
                  target_arg_ends[arg_index],
                  target_arg_ctor_bindings[arg_index]
                );
              } else {
                bind_ok = raw_emit_bind_expr_name(
                  source,
                  decls,
                  decl_count,
                  env,
                  inline_stack,
                  inline_depth + 1u,
                  pat_name,
                  target_arg_starts[arg_index],
                  target_arg_ends[arg_index]
                );
              }
            }
            if (!bind_ok) {
              env->expr_count = saved_expr_count;
              env->ctor_binding_count = saved_ctor_binding_count;
              return 0;
            }
          }
          arg_index += 1u;
          pattern_cursor = skip_expr_ws(source, pat_end, arrow_at);
        }
        if (arg_index != target_arg_count) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        matched = 1;
      } else if (first_end == pat_start + 1u && mem[pat_start] == '_') {
        matched = 1;
      } else if (first.ok && next == arrow_at) {
        if (!raw_emit_bind_expr_name(
              source,
              decls,
              decl_count,
              env,
              inline_stack,
              inline_depth + 1u,
              first,
              target_start,
              target_end)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        matched = 1;
      } else {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      if (matched) {
        int ok = raw_resolve_direct_constructor_target(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth + 1u,
          arrow_at + 2u,
          line_end,
          ctor_name_out,
          arg_starts,
          arg_ends,
          arg_ctor_bindings,
          max_args,
          argc_out,
          depth + 1u
        );
        if (ok) {
          for (uint32_t i = 0u; i < *argc_out; i += 1u) {
            uint32_t resolved_start = 0u;
            uint32_t resolved_end = 0u;
            if (raw_resolve_bound_expr_span(
                  source,
                  env,
                  arg_starts[i],
                  arg_ends[i],
                  &resolved_start,
                  &resolved_end,
                  0u)) {
              arg_starts[i] = resolved_start;
              arg_ends[i] = resolved_end;
            }
            if (arg_ctor_bindings &&
                raw_capture_constructor_binding(
                  source,
                  decls,
                  decl_count,
                  env,
                  inline_stack,
                  inline_depth + 1u,
                  arg_starts[i],
                  arg_ends[i],
                  &arg_ctor_bindings[i],
                  depth + 1u) &&
                arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
              raw_normalize_ctor_binding(
                source,
                decls,
                decl_count,
                env,
                inline_stack,
                inline_depth + 1u,
                arg_ctor_bindings[i],
                arg_starts[i],
                arg_ends[i],
                depth + 1u
              );
              RawCtorBinding *binding = &env->ctor_bindings[arg_ctor_bindings[i]];
              arg_starts[i] = binding->expr_start;
              arg_ends[i] = binding->expr_end;
            }
          }
        }
        env->expr_count = saved_expr_count;
        if (!ok) {
          env->ctor_binding_count = saved_ctor_binding_count;
        }
        return ok;
      }
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      line_start = skip_expr_ws(source, next_line, end);
    }
    return 0;
  }
  if (span_matches_keyword(source, start, end, "let")) {
    uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
    uint32_t cursor_at = start + 3u;
    uint32_t saved_expr_count = env->expr_count;
    uint32_t saved_ctor_binding_count = env->ctor_binding_count;
    while (cursor_at < end) {
      cursor_at = skip_expr_ws(source, cursor_at, end);
      if (cursor_at >= end) {
        break;
      }
      if (span_matches_keyword(source, cursor_at, end, "in")) {
        uint32_t in_line_end = source_line_end(source, cursor_at);
        if (in_line_end > end) {
          in_line_end = end;
        }
        int ok = raw_resolve_direct_constructor_target(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth,
          cursor_at + 2u,
          in_line_end,
          ctor_name_out,
          arg_starts,
          arg_ends,
          arg_ctor_bindings,
          max_args,
          argc_out,
          depth + 1u
        );
        env->expr_count = saved_expr_count;
        if (!ok) {
          env->ctor_binding_count = saved_ctor_binding_count;
        }
        return ok;
      }
      uint32_t name_next = cursor_at;
      NameSpan name = parse_simple_name_token(source, cursor_at, end, &name_next);
      uint32_t line_end = source_line_end(source, cursor_at);
      if (!name.ok || line_end > end) {
        if (line_end > end) {
          line_end = end;
        } else {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
      }
      uint32_t eq_at = skip_expr_ws(source, name_next, line_end);
      if (eq_at >= line_end || mem[eq_at] != '=') {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      uint32_t value_start = eq_at + 1u;
      uint32_t binding_end = line_end;
      uint32_t next_cursor = line_end;
      int found_in = 0;
      if (!find_let_binding_split(
            source,
            value_start,
            end,
            line_end,
            &binding_end,
            &next_cursor,
            &found_in
          ) ||
          !raw_emit_bind_expr_name(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            name,
            value_start,
            binding_end
          )) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      if (found_in) {
        int ok = raw_resolve_direct_constructor_target(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth,
          next_cursor,
          end,
          ctor_name_out,
          arg_starts,
          arg_ends,
          arg_ctor_bindings,
          max_args,
          argc_out,
          depth + 1u
        );
        env->expr_count = saved_expr_count;
        if (!ok) {
          env->ctor_binding_count = saved_ctor_binding_count;
        }
        return ok;
      }
      cursor_at = next_cursor;
    }
    env->expr_count = saved_expr_count;
    env->ctor_binding_count = saved_ctor_binding_count;
    return 0;
  }
  NameSpan head = missing_name_span();
  uint32_t argc = 0u;
  if (!raw_parse_apply_span(
        source,
        start,
        end,
        &head,
        arg_starts,
        arg_ends,
        max_args,
        &argc)) {
    return 0;
  }
  if (namespan_starts_with_upper(source, head)) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      uint32_t resolved_start = 0u;
      uint32_t resolved_end = 0u;
      if (raw_resolve_bound_expr_span(source, env, arg_starts[i], arg_ends[i], &resolved_start, &resolved_end, 0u)) {
        arg_starts[i] = resolved_start;
        arg_ends[i] = resolved_end;
      }
      if (arg_ctor_bindings &&
          raw_capture_constructor_binding(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            arg_starts[i],
            arg_ends[i],
            &arg_ctor_bindings[i],
            depth + 1u
          ) &&
          arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
        raw_normalize_ctor_binding(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth + 1u,
          arg_ctor_bindings[i],
          arg_starts[i],
          arg_ends[i],
          depth + 1u
        );
        RawCtorBinding *binding = &env->ctor_bindings[arg_ctor_bindings[i]];
        arg_starts[i] = binding->expr_start;
        arg_ends[i] = binding->expr_end;
      }
    }
    *ctor_name_out = head;
    *argc_out = argc;
    return 1;
  }
  if (raw_emit_inline_stack_has(inline_stack, inline_depth, head)) {
    return 0;
  }
  int decl_index = find_decl_index_by_name(decls, decl_count, head);
  if (decl_index < 0) {
    return 0;
  }
  FnDecl decl = decls[(uint32_t) decl_index];
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = collect_decl_params(source, decl, params, MAX_EVAL_ARGS);
  if (param_count != argc || env->expr_count + argc > MAX_RAW_EMIT_EXPR_BINDINGS ||
      inline_depth >= MAX_RAW_EMIT_INLINE_DEPTH) {
    return 0;
  }
  uint32_t saved_expr_count = env->expr_count;
  uint32_t saved_ctor_binding_count = env->ctor_binding_count;
  for (uint32_t i = 0u; i < argc; i += 1u) {
    uint32_t original_next = arg_starts[i];
    uint32_t original_ctor_binding = MISSING_CTOR_BINDING;
    uint32_t original_bound_start = 0u;
    uint32_t original_bound_end = 0u;
    NameSpan original_name = parse_simple_name_token(source, arg_starts[i], arg_ends[i], &original_next);
    if (original_name.ok && original_next == arg_ends[i]) {
      raw_emit_lookup_expr_binding(
        original_name,
        env,
        &original_bound_start,
        &original_bound_end,
        &original_ctor_binding
      );
    }
    uint32_t resolved_start = arg_starts[i];
    uint32_t resolved_end = arg_ends[i];
    raw_resolve_bound_expr_span(
      source,
      env,
      arg_starts[i],
      arg_ends[i],
      &resolved_start,
      &resolved_end,
      0u
    );
    if (original_ctor_binding != MISSING_CTOR_BINDING) {
      if (!raw_emit_bind_expr_name_with_ctor(
            env,
            params[i],
            original_bound_start,
            original_bound_end,
            original_ctor_binding)) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      continue;
    }
    {
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (raw_capture_constructor_binding(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            arg_starts[i],
            arg_ends[i],
            &ctor_binding,
            depth + 1u)) {
        if (!raw_emit_bind_expr_name_with_ctor(
              env,
              params[i],
              resolved_start,
              resolved_end,
              ctor_binding)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        continue;
      }
    }
    if (!raw_emit_bind_expr_name(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth + 1u,
          params[i],
          resolved_start,
          resolved_end)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
  }
  inline_stack[inline_depth] = head;
  uint32_t expr_end = decl_expression_end(source, decls, decl_count, (uint32_t) decl_index);
  int ok = raw_resolve_direct_constructor_target(
    source,
    decls,
    decl_count,
    env,
    inline_stack,
    inline_depth + 1u,
    decl.body_start,
    expr_end,
    ctor_name_out,
    arg_starts,
    arg_ends,
    arg_ctor_bindings,
    max_args,
    argc_out,
    depth + 1u
  );
  if (ok) {
    for (uint32_t i = 0u; i < *argc_out; i += 1u) {
      uint32_t resolved_start = 0u;
      uint32_t resolved_end = 0u;
      if (raw_resolve_bound_expr_span(
            source,
            env,
            arg_starts[i],
            arg_ends[i],
            &resolved_start,
            &resolved_end,
            0u)) {
        arg_starts[i] = resolved_start;
        arg_ends[i] = resolved_end;
      }
      if (arg_ctor_bindings &&
          raw_capture_constructor_binding(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            arg_starts[i],
            arg_ends[i],
            &arg_ctor_bindings[i],
            depth + 1u
          ) &&
          arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
        raw_normalize_ctor_binding(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth + 1u,
          arg_ctor_bindings[i],
          arg_starts[i],
          arg_ends[i],
          depth + 1u
        );
        RawCtorBinding *binding = &env->ctor_bindings[arg_ctor_bindings[i]];
        arg_starts[i] = binding->expr_start;
        arg_ends[i] = binding->expr_end;
      }
    }
  }
  env->expr_count = saved_expr_count;
  if (!ok) {
    env->ctor_binding_count = saved_ctor_binding_count;
  }
  return ok;
}

static int raw_capture_constructor_binding(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint32_t start,
  uint32_t end,
  uint32_t *binding_index_out,
  uint32_t depth
) {
  if (!env || !binding_index_out || depth > MAX_RAW_EMIT_INLINE_DEPTH) {
    return 0;
  }
  start = skip_expr_ws(source, start, end);
  end = trim_expr_end(source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_is_wrapped_parens(source, start, end)) {
    return raw_capture_constructor_binding(
      source,
      decls,
      decl_count,
      env,
      inline_stack,
      inline_depth,
      start + 1u,
      end - 1u,
      binding_index_out,
      depth + 1u
    );
  }
  {
    uint32_t next = start;
    NameSpan simple = parse_simple_name_token(source, start, end, &next);
    if (simple.ok && next == end) {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (raw_emit_lookup_expr_binding(simple, env, &bound_start, &bound_end, &ctor_binding) &&
          ctor_binding != MISSING_CTOR_BINDING) {
        *binding_index_out = ctor_binding;
        return 1;
      }
    }
  }
  if (env->ctor_binding_count >= MAX_RAW_EMIT_CTOR_BINDINGS) {
    return 0;
  }
  NameSpan ctor_name = missing_name_span();
  uint32_t arg_starts[MAX_EVAL_ARGS] = {0};
  uint32_t arg_ends[MAX_EVAL_ARGS] = {0};
  uint32_t arg_ctor_bindings[MAX_EVAL_ARGS];
  init_ctor_binding_array(arg_ctor_bindings, MAX_EVAL_ARGS);
  uint32_t argc = 0u;
  if (!raw_resolve_direct_constructor_target(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        inline_depth,
        start,
        end,
        &ctor_name,
        arg_starts,
        arg_ends,
        arg_ctor_bindings,
        MAX_EVAL_ARGS,
        &argc,
        depth + 1u
      )) {
    return 0;
  }
  uint32_t binding_index = env->ctor_binding_count;
  env->ctor_binding_count += 1u;
  RawCtorBinding *binding = &env->ctor_bindings[binding_index];
  binding->ctor_name = ctor_name;
  binding->expr_start = start;
  binding->expr_end = end;
  binding->arg_count = argc;
  for (uint32_t i = 0u; i < argc; i += 1u) {
    binding->arg_starts[i] = arg_starts[i];
    binding->arg_ends[i] = arg_ends[i];
    binding->arg_ctor_bindings[i] = arg_ctor_bindings[i];
    if (
      binding->arg_ctor_bindings[i] == MISSING_CTOR_BINDING &&
      raw_capture_constructor_binding(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        inline_depth,
        arg_starts[i],
        arg_ends[i],
        &binding->arg_ctor_bindings[i],
        depth + 1u
      )
    ) {
      RawCtorBinding *child = &env->ctor_bindings[binding->arg_ctor_bindings[i]];
      binding->arg_starts[i] = child->expr_start;
      binding->arg_ends[i] = child->expr_end;
    }
  }
  *binding_index_out = binding_index;
  return 1;
}

static void raw_normalize_ctor_binding(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint32_t binding_index,
  uint32_t expr_start,
  uint32_t expr_end,
  uint32_t depth
) {
  if (!env || binding_index == MISSING_CTOR_BINDING ||
      binding_index >= env->ctor_binding_count ||
      depth > MAX_RAW_EMIT_INLINE_DEPTH) {
    return;
  }
  RawCtorBinding *binding = &env->ctor_bindings[binding_index];
  binding->expr_start = expr_start;
  binding->expr_end = expr_end;
  for (uint32_t i = 0u; i < binding->arg_count; i += 1u) {
    uint32_t resolved_start = binding->arg_starts[i];
    uint32_t resolved_end = binding->arg_ends[i];
    if (raw_resolve_bound_expr_span(
          source,
          env,
          binding->arg_starts[i],
          binding->arg_ends[i],
          &resolved_start,
          &resolved_end,
          0u)) {
      binding->arg_starts[i] = resolved_start;
      binding->arg_ends[i] = resolved_end;
    }
    if (binding->arg_ctor_bindings[i] == MISSING_CTOR_BINDING) {
      raw_capture_constructor_binding(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        inline_depth,
        binding->arg_starts[i],
        binding->arg_ends[i],
        &binding->arg_ctor_bindings[i],
        depth + 1u
      );
    }
    if (binding->arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
      raw_normalize_ctor_binding(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        inline_depth,
        binding->arg_ctor_bindings[i],
        binding->arg_starts[i],
        binding->arg_ends[i],
        depth + 1u
      );
      RawCtorBinding *child = &env->ctor_bindings[binding->arg_ctor_bindings[i]];
      binding->arg_starts[i] = child->expr_start;
      binding->arg_ends[i] = child->expr_end;
    }
  }
}

static int raw_emit_binding_like_line(
  Segment source,
  uint32_t start,
  uint32_t end,
  uint32_t *eq_at_out
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t cursor = skip_expr_ws(source, start, end);
  uint32_t name_next = cursor;
  NameSpan name = parse_simple_name_token(source, cursor, end, &name_next);
  if (!name.ok) {
    return 0;
  }
  uint32_t eq_at = skip_expr_ws(source, name_next, end);
  if (eq_at >= end || mem[eq_at] != '=') {
    return 0;
  }
  if (eq_at_out) {
    *eq_at_out = eq_at;
  }
  return 1;
}

static uint32_t raw_emit_find_top_level_binary_op(
  Segment source,
  uint32_t start,
  uint32_t end,
  uint32_t precedence
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = end; at > start; at -= 1u) {
    uint32_t idx = at - 1u;
    uint8_t c = mem[idx];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == ')') {
      depth += 1u;
      continue;
    }
    if (c == '(') {
      if (depth > 0u) {
        depth -= 1u;
      }
      continue;
    }
    if (depth != 0u) {
      continue;
    }
    if (precedence == 0u) {
      if (idx > start) {
        uint8_t prev = mem[idx - 1u];
        if ((prev == '&' && c == '&') || (prev == '|' && c == '|')) {
          return idx - 1u;
        }
      }
    } else if (precedence == 1u) {
      if (idx > start) {
        uint8_t prev = mem[idx - 1u];
        if ((prev == '=' || prev == '!') && c == '=') {
          return idx - 1u;
        }
        if ((prev == '<' || prev == '>') && c == '=') {
          return idx - 1u;
        }
      }
      if (c == '<' || c == '>') {
        return idx;
      }
    } else if (precedence == 2u) {
      if (idx > start && c == '.') {
        uint8_t prev = mem[idx - 1u];
        if (prev == '+' || prev == '-' || prev == '*' || prev == '/' || prev == '%') {
          uint32_t left_end = trim_expr_end(source, start, idx - 1u);
          uint32_t right_start = skip_expr_ws(source, idx + 1u, end);
          if (left_end > start && right_start < end) {
            return idx - 1u;
          }
        }
      }
    } else if (precedence == 3u) {
      if (c == '+' || c == '-') {
        uint32_t left_end = trim_expr_end(source, start, idx);
        uint32_t right_start = skip_expr_ws(source, idx + 1u, end);
        if (left_end > start && right_start < end) {
          return idx;
        }
      }
    } else if (precedence == 4u) {
      if (c == '*' || c == '/' || c == '%') {
        return idx;
      }
    }
  }
  return end;
}

static int raw_emit_apply_named_builtin(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan head,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  if (
    namespan_equals_literal(head, "add") ||
    namespan_equals_literal(head, "sub") ||
    namespan_equals_literal(head, "mul") ||
    namespan_equals_literal(head, "div") ||
    namespan_equals_literal(head, "mod") ||
    namespan_equals_literal(head, "eq") ||
    namespan_equals_literal(head, "ne") ||
    namespan_equals_literal(head, "lt") ||
    namespan_equals_literal(head, "le") ||
    namespan_equals_literal(head, "gt") ||
    namespan_equals_literal(head, "ge") ||
    namespan_equals_literal(head, "and") ||
    namespan_equals_literal(head, "or")
  ) {
    if (argc != 2u) {
      return 0;
    }
    if (
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[0],
        arg_ends[0],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[1],
        arg_ends[1],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      )
    ) {
      return 0;
    }
    if (namespan_equals_literal(head, "add")) {
      return raw_emit_append_byte(out, cursor, limit, 0x6au);
    }
    if (namespan_equals_literal(head, "sub")) {
      return raw_emit_append_byte(out, cursor, limit, 0x6bu);
    }
    if (namespan_equals_literal(head, "mul")) {
      return raw_emit_append_byte(out, cursor, limit, 0x6cu);
    }
    if (namespan_equals_literal(head, "div")) {
      return raw_emit_append_byte(out, cursor, limit, 0x6du);
    }
    if (namespan_equals_literal(head, "mod")) {
      return raw_emit_append_byte(out, cursor, limit, 0x6fu);
    }
    if (namespan_equals_literal(head, "eq")) {
      return raw_emit_append_byte(out, cursor, limit, 0x46u);
    }
    if (namespan_equals_literal(head, "ne")) {
      return raw_emit_append_byte(out, cursor, limit, 0x47u);
    }
    if (namespan_equals_literal(head, "lt")) {
      return raw_emit_append_byte(out, cursor, limit, 0x48u);
    }
    if (namespan_equals_literal(head, "gt")) {
      return raw_emit_append_byte(out, cursor, limit, 0x4au);
    }
    if (namespan_equals_literal(head, "le")) {
      return raw_emit_append_byte(out, cursor, limit, 0x4cu);
    }
    if (namespan_equals_literal(head, "ge")) {
      return raw_emit_append_byte(out, cursor, limit, 0x4eu);
    }
    if (namespan_equals_literal(head, "and")) {
      return raw_emit_append_byte(out, cursor, limit, 0x71u);
    }
    if (namespan_equals_literal(head, "or")) {
      return raw_emit_append_byte(out, cursor, limit, 0x72u);
    }
  }
  if (namespan_equals_literal(head, "not") && argc == 1u) {
    if (!raw_emit_expr_to_wasm(
          source,
          decls,
          decl_count,
          arg_starts[0],
          arg_ends[0],
          env,
          inline_stack,
          inline_depth + 1u,
          out,
          cursor,
          limit
        )) {
      return 0;
    }
    return raw_emit_append_byte(out, cursor, limit, 0x45u);
  }
  if (
    (namespan_equals_literal(head, "slice_len") ||
      namespan_equals_literal(head, "slice_len_raw") ||
      namespan_equals_literal(head, "slice_data_ptr")) && argc == 1u
  ) {
    if (!raw_emit_expr_to_wasm(
          source,
          decls,
          decl_count,
          arg_starts[0],
          arg_ends[0],
          env,
          inline_stack,
          inline_depth + 1u,
          out,
          cursor,
          limit
        ) ||
        !raw_emit_append_byte(out, cursor, limit, 0x28u) ||
        !raw_emit_append_var_u32(out, cursor, limit, 2u) ||
        !raw_emit_append_var_u32(
          out,
          cursor,
          limit,
          namespan_equals_literal(head, "slice_data_ptr") ? 0u : 4u
        )) {
      return 0;
    }
    return 1;
  }
  if (namespan_equals_literal(head, "slice_get_u8") && argc == 2u) {
    if (
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[0],
        arg_ends[0],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x28u) ||
      !raw_emit_append_var_u32(out, cursor, limit, 2u) ||
      !raw_emit_append_var_u32(out, cursor, limit, 0u) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[1],
        arg_ends[1],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x6au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x2du) ||
      !raw_emit_append_var_u32(out, cursor, limit, 0u) ||
      !raw_emit_append_var_u32(out, cursor, limit, 0u)
    ) {
      return 0;
    }
    return 1;
  }
  if (namespan_equals_literal(head, "slice_new_u8") && argc == 1u) {
    uint32_t len_local = 0u;
    uint32_t desc_local = 0u;
    uint32_t pages_local = 0u;
    if (
      !raw_emit_reserve_temp_local(env, &len_local) ||
      !raw_emit_reserve_temp_local(env, &desc_local) ||
      !raw_emit_reserve_temp_local(env, &pages_local) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[0],
        arg_ends[0],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, len_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, len_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 65543) ||
      !raw_emit_append_byte(out, cursor, limit, 0x6au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 16) ||
      !raw_emit_append_byte(out, cursor, limit, 0x76u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, pages_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x3fu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 16) ||
      !raw_emit_append_byte(out, cursor, limit, 0x74u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, desc_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, pages_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x40u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x1au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, desc_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, desc_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 8) ||
      !raw_emit_append_byte(out, cursor, limit, 0x6au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x36u) ||
      !raw_emit_append_var_u32(out, cursor, limit, 2u) ||
      !raw_emit_append_var_u32(out, cursor, limit, 0u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, desc_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, len_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x36u) ||
      !raw_emit_append_var_u32(out, cursor, limit, 2u) ||
      !raw_emit_append_var_u32(out, cursor, limit, 4u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, desc_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 8) ||
      !raw_emit_append_byte(out, cursor, limit, 0x6au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 0) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, len_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0xfcu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x0bu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, desc_local)
    ) {
      return 0;
    }
    return 1;
  }
  if (namespan_equals_literal(head, "region_mark") && argc == 1u) {
    return raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      arg_starts[0],
      arg_ends[0],
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
  }
  if (namespan_equals_literal(head, "region_reset") && argc == 1u) {
    return raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      arg_starts[0],
      arg_ends[0],
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
  }
  if (namespan_equals_literal(head, "region_alloc") && argc == 2u) {
    uint32_t size_local = 0u;
    uint32_t ptr_local = 0u;
    uint32_t pages_local = 0u;
    if (
      !raw_emit_reserve_temp_local(env, &size_local) ||
      !raw_emit_reserve_temp_local(env, &ptr_local) ||
      !raw_emit_reserve_temp_local(env, &pages_local) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[0],
        arg_ends[0],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, size_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, size_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 65535) ||
      !raw_emit_append_byte(out, cursor, limit, 0x6au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 16) ||
      !raw_emit_append_byte(out, cursor, limit, 0x76u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, pages_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x3fu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 16) ||
      !raw_emit_append_byte(out, cursor, limit, 0x74u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, ptr_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, pages_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x40u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x1au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, ptr_local)
    ) {
      return 0;
    }
    return 1;
  }
  if (namespan_equals_literal(head, "memcpy_u8") && argc == 3u) {
    uint32_t dst_local = 0u;
    if (
      !raw_emit_reserve_temp_local(env, &dst_local) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[0],
        arg_ends[0],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, dst_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, dst_local) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[1],
        arg_ends[1],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[2],
        arg_ends[2],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0xfcu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x0au) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, dst_local)
    ) {
      return 0;
    }
    return 1;
  }
  if (namespan_equals_literal(head, "memset_u8") && argc == 3u) {
    uint32_t dst_local = 0u;
    if (
      !raw_emit_reserve_temp_local(env, &dst_local) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[0],
        arg_ends[0],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, dst_local) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, dst_local) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[1],
        arg_ends[1],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[2],
        arg_ends[2],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0xfcu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x0bu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x00u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x20u) ||
      !raw_emit_append_var_u32(out, cursor, limit, dst_local)
    ) {
      return 0;
    }
    return 1;
  }
  if (namespan_equals_literal(head, "if") && argc == 3u) {
    uint32_t zero_local = 0u;
    if (
      !raw_emit_reserve_temp_local(env, &zero_local) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[0],
        arg_ends[0],
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x04u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x7fu) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 0) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, zero_local) ||
      !raw_emit_if_branch_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[1],
        arg_ends[1],
        env,
        inline_stack,
        inline_depth + 1u,
        zero_local,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x05u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
      !raw_emit_append_var_s32(out, cursor, limit, 0) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, zero_local) ||
      !raw_emit_if_branch_to_wasm(
        source,
        decls,
        decl_count,
        arg_starts[2],
        arg_ends[2],
        env,
        inline_stack,
        inline_depth + 1u,
        zero_local,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x0bu)
    ) {
      return 0;
    }
    return 1;
  }
  return 0;
}

static int raw_emit_inline_named_decl(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan head,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  uint32_t decl_index = 0u;
  if (!raw_emit_find_decl(decls, decl_count, head, &decl_index)) {
    return 0;
  }
  if (inline_depth >= MAX_RAW_EMIT_INLINE_DEPTH || raw_emit_inline_stack_has(inline_stack, inline_depth, head)) {
    return 0;
  }
  FnDecl decl = decls[decl_index];
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = collect_decl_params(source, decl, params, MAX_EVAL_ARGS);
  if (param_count != argc) {
    return 0;
  }
  uint32_t saved_expr_count = env->expr_count;
  uint32_t saved_ctor_binding_count = env->ctor_binding_count;
  if (env->expr_count + argc > MAX_RAW_EMIT_EXPR_BINDINGS) {
    return 0;
  }
  for (uint32_t i = 0u; i < argc; i += 1u) {
    uint32_t original_next = arg_starts[i];
    uint32_t original_ctor_binding = MISSING_CTOR_BINDING;
    uint32_t original_bound_start = 0u;
    uint32_t original_bound_end = 0u;
    NameSpan original_name = parse_simple_name_token(source, arg_starts[i], arg_ends[i], &original_next);
    if (original_name.ok && original_next == arg_ends[i]) {
      raw_emit_lookup_expr_binding(
        original_name,
        env,
        &original_bound_start,
        &original_bound_end,
        &original_ctor_binding
      );
    }
    uint32_t resolved_start = arg_starts[i];
    uint32_t resolved_end = arg_ends[i];
    raw_resolve_bound_expr_span(
      source,
      env,
      arg_starts[i],
      arg_ends[i],
      &resolved_start,
      &resolved_end,
      0u
    );
    if (original_ctor_binding != MISSING_CTOR_BINDING) {
      if (!raw_emit_bind_expr_name_with_ctor(
            env,
            params[i],
            original_bound_start,
            original_bound_end,
            original_ctor_binding)) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      continue;
    }
    {
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (raw_capture_constructor_binding(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            arg_starts[i],
            arg_ends[i],
            &ctor_binding,
            0u)) {
        if (!raw_emit_bind_expr_name_with_ctor(
              env,
              params[i],
              resolved_start,
              resolved_end,
              ctor_binding)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        continue;
      }
    }
    if (!raw_emit_bind_expr_name(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          inline_depth + 1u,
          params[i],
          resolved_start,
          resolved_end)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
  }
  uint32_t function_end = decl_function_end(decls, decl_count, decl_index, source.len);
  if (is_simple_clause_block(source, decl, function_end)) {
    env->expr_count = saved_expr_count;
    env->ctor_binding_count = saved_ctor_binding_count;
    return 0;
  }
  uint32_t expr_end = decl_expression_end(source, decls, decl_count, decl_index);
  inline_stack[inline_depth] = head;
  int ok = raw_emit_expr_to_wasm(
    source,
    decls,
    decl_count,
    decl.body_start,
    expr_end,
    env,
    inline_stack,
    inline_depth + 1u,
    out,
    cursor,
    limit
  );
  env->expr_count = saved_expr_count;
  env->ctor_binding_count = saved_ctor_binding_count;
  return ok;
}

static int raw_emit_atom_to_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  start = skip_expr_ws(source, start, end);
  end = trim_expr_end(source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_is_wrapped_parens(source, start, end)) {
    return raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      start + 1u,
      end - 1u,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
  }
  {
    int32_t int_value = 0;
    uint32_t next = start;
    if (parse_signed_int_literal(source, start, end, &int_value, &next) && next == end) {
      return raw_emit_append_byte(out, cursor, limit, 0x41u) &&
        raw_emit_append_var_s32(out, cursor, limit, int_value);
    }
  }
  {
    uint32_t next = start;
    NameSpan name = parse_simple_name_token(source, start, end, &next);
    if (!name.ok || next != end) {
      return 0;
    }
    if (namespan_equals_literal(name, "true") || namespan_equals_literal(name, "True")) {
      return raw_emit_append_byte(out, cursor, limit, 0x41u) &&
        raw_emit_append_var_s32(out, cursor, limit, 1);
    }
    if (namespan_equals_literal(name, "false") || namespan_equals_literal(name, "False")) {
      return raw_emit_append_byte(out, cursor, limit, 0x41u) &&
        raw_emit_append_var_s32(out, cursor, limit, 0);
    }
    {
      uint32_t local_index = 0u;
      if (raw_emit_lookup_local(name, env, &local_index)) {
        return raw_emit_append_byte(out, cursor, limit, 0x20u) &&
          raw_emit_append_var_u32(out, cursor, limit, local_index);
      }
    }
    {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      if (raw_emit_lookup_expr_binding(name, env, &bound_start, &bound_end, NULL)) {
        return raw_emit_expr_to_wasm(
          source,
          decls,
          decl_count,
          bound_start,
          bound_end,
          env,
          inline_stack,
          inline_depth + 1u,
          out,
          cursor,
          limit
        );
      }
    }
    {
      uint32_t decl_index = 0u;
      if (raw_emit_find_decl(decls, decl_count, name, &decl_index)) {
        if (raw_emit_inline_stack_has(inline_stack, inline_depth, name)) {
          return 0;
        }
        NameSpan params[MAX_EVAL_ARGS] = {0};
        uint32_t param_count = collect_decl_params(source, decls[decl_index], params, MAX_EVAL_ARGS);
        if (param_count == 0u) {
          EvalValue value = eval_decl_by_name_extended(source, decls, decl_count, name, NULL, 0u, 0u);
          if (value.ok && value.kind == EVAL_VALUE_INT) {
            return raw_emit_append_byte(out, cursor, limit, 0x41u) &&
              raw_emit_append_var_s32(out, cursor, limit, value.int_value);
          }
        }
      }
    }
  }
  return 0;
}

static int raw_emit_let_expr_to_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  if (!span_matches_keyword(source, start, end, "let")) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t cursor_at = start + 3u;
  uint32_t saved_local_count = env->count;
  uint32_t saved_next_local_index = env->next_local_index;
  uint32_t saved_expr_count = env->expr_count;
  uint32_t saved_ctor_binding_count = env->ctor_binding_count;
  while (cursor_at < end) {
    cursor_at = skip_expr_ws(source, cursor_at, end);
    if (cursor_at >= end) {
      break;
    }
    if (span_matches_keyword(source, cursor_at, end, "in")) {
      uint32_t in_line_end = source_line_end(source, cursor_at);
      if (in_line_end > end) {
        in_line_end = end;
      }
      cursor_at += 2u;
      int ok = raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        cursor_at,
        in_line_end,
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      );
      env->count = saved_local_count;
      if (!ok) {
        env->next_local_index = saved_next_local_index;
      }
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return ok;
    }
    uint32_t name_next = cursor_at;
    NameSpan name = parse_simple_name_token(source, cursor_at, end, &name_next);
    uint32_t line_end = source_line_end(source, cursor_at);
    if (line_end > end) {
      line_end = end;
    }
    uint32_t eq_at = name_next;
    int ctor_pattern = 0;
    NameSpan ctor_name = missing_name_span();
    uint32_t ctor_pat_arg_starts[MAX_EVAL_ARGS];
    uint32_t ctor_pat_arg_ends[MAX_EVAL_ARGS];
    uint32_t ctor_pat_arg_count = 0u;
    if (name.ok) {
      ctor_name = name;
      uint32_t lookahead = skip_expr_ws(source, name_next, line_end);
      if (namespan_starts_with_upper(source, name) &&
          lookahead < line_end && mem[lookahead] != '=') {
        ctor_pattern = 1;
        uint32_t pattern_end = lookahead;
        while (pattern_end < line_end && mem[pattern_end] != '=') {
          pattern_end += 1u;
        }
        uint32_t pat_cursor = lookahead;
        while (pat_cursor < pattern_end) {
          uint32_t pat_end = parse_expr_atom_end(source, pat_cursor, pattern_end);
          if (pat_end <= pat_cursor) {
            break;
          }
          if (ctor_pat_arg_count >= MAX_EVAL_ARGS) {
            env->count = saved_local_count;
            env->next_local_index = saved_next_local_index;
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          ctor_pat_arg_starts[ctor_pat_arg_count] = pat_cursor;
          ctor_pat_arg_ends[ctor_pat_arg_count] = pat_end;
          ctor_pat_arg_count += 1u;
          pat_cursor = skip_expr_ws(source, pat_end, line_end);
        }
      }
    }
    if ((!name.ok && !ctor_pattern) || (!ctor_pattern && env->count >= MAX_EVAL_LOCALS)) {
      env->count = saved_local_count;
      env->next_local_index = saved_next_local_index;
      env->expr_count = saved_expr_count;
      return 0;
    }
    eq_at = skip_expr_ws(source, name_next, end);
    if (eq_at >= end || mem[eq_at] != '=') {
      env->count = saved_local_count;
      env->next_local_index = saved_next_local_index;
      env->expr_count = saved_expr_count;
      return 0;
    }
    uint32_t value_start = eq_at + 1u;
    uint32_t binding_end = line_end;
    uint32_t next_cursor = line_end;
    int found_in = 0;
    if (!find_let_binding_split(
          source,
          value_start,
          end,
          line_end,
          &binding_end,
          &next_cursor,
          &found_in
        )) {
      env->count = saved_local_count;
      env->next_local_index = saved_next_local_index;
      env->expr_count = saved_expr_count;
      return 0;
    }
    cursor_at = found_in ? binding_end : next_cursor;
    if (
      ctor_pattern
    ) {
      NameSpan target_ctor = missing_name_span();
      uint32_t target_arg_starts[MAX_EVAL_ARGS];
      uint32_t target_arg_ends[MAX_EVAL_ARGS];
      uint32_t target_arg_ctor_bindings[MAX_EVAL_ARGS];
      init_ctor_binding_array(target_arg_ctor_bindings, MAX_EVAL_ARGS);
      uint32_t target_arg_count = 0u;
      if (!raw_resolve_direct_constructor_target(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            value_start,
            binding_end,
            &target_ctor,
            target_arg_starts,
            target_arg_ends,
            target_arg_ctor_bindings,
            MAX_EVAL_ARGS,
            &target_arg_count,
            0u) ||
          !names_equal(ctor_name, target_ctor) ||
          target_arg_count != ctor_pat_arg_count) {
        env->count = saved_local_count;
        env->next_local_index = saved_next_local_index;
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      for (uint32_t i = 0u; i < ctor_pat_arg_count; i += 1u) {
        uint32_t pat_start = skip_expr_ws(source, ctor_pat_arg_starts[i], ctor_pat_arg_ends[i]);
        uint32_t pat_end = trim_expr_end(source, pat_start, ctor_pat_arg_ends[i]);
        if (pat_start >= pat_end) {
          env->count = saved_local_count;
          env->next_local_index = saved_next_local_index;
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        if (pat_end == pat_start + 1u && mem[pat_start] == '_') {
          continue;
        }
        uint32_t pat_next = pat_start;
        NameSpan pat_name = parse_simple_name_token(source, pat_start, pat_end, &pat_next);
        int bind_ok = 0;
        if (pat_name.ok && pat_next == pat_end) {
          if (target_arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
            bind_ok = raw_emit_bind_expr_name_with_ctor(
              env,
              pat_name,
              target_arg_starts[i],
              target_arg_ends[i],
              target_arg_ctor_bindings[i]
            );
          } else {
            bind_ok = raw_emit_bind_expr_name(
              source,
              decls,
              decl_count,
              env,
              inline_stack,
              inline_depth + 1u,
              pat_name,
              target_arg_starts[i],
              target_arg_ends[i]
            );
          }
        }
        if (!bind_ok) {
          env->count = saved_local_count;
          env->next_local_index = saved_next_local_index;
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
      }
      if (found_in) {
        int ok = raw_emit_expr_to_wasm(
          source,
          decls,
          decl_count,
          next_cursor,
          line_end,
          env,
          inline_stack,
          inline_depth + 1u,
          out,
          cursor,
          limit
        );
        env->count = saved_local_count;
        if (!ok) {
          env->next_local_index = saved_next_local_index;
        }
        env->expr_count = saved_expr_count;
        if (!ok) {
          env->ctor_binding_count = saved_ctor_binding_count;
        }
        return ok;
      }
      continue;
    }
    {
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (raw_capture_constructor_binding(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            value_start,
            binding_end,
            &ctor_binding,
            0u
          )) {
        RawCtorBinding *binding = &env->ctor_bindings[ctor_binding];
        if (!raw_emit_bind_expr_name_with_ctor(
              env,
              name,
              binding->expr_start,
              binding->expr_end,
              ctor_binding
            )) {
          env->count = saved_local_count;
          env->next_local_index = saved_next_local_index;
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        if (found_in) {
          int ok = raw_emit_expr_to_wasm(
            source,
            decls,
            decl_count,
            next_cursor,
            line_end,
            env,
            inline_stack,
            inline_depth + 1u,
            out,
            cursor,
            limit
          );
          env->count = saved_local_count;
          if (!ok) {
            env->next_local_index = saved_next_local_index;
          }
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return ok;
        }
        continue;
      }
    }
    if (
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        value_start,
        binding_end,
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x21u) ||
      !raw_emit_append_var_u32(out, cursor, limit, env->next_local_index)
    ) {
      env->count = saved_local_count;
      env->next_local_index = saved_next_local_index;
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
    env->names[env->count] = name;
    env->indices[env->count] = env->next_local_index;
    env->count += 1u;
    env->next_local_index += 1u;
    if (found_in) {
      int ok = raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        next_cursor,
        line_end,
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      );
      env->count = saved_local_count;
      if (!ok) {
        env->next_local_index = saved_next_local_index;
      }
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return ok;
    }
  }
  env->count = saved_local_count;
  env->next_local_index = saved_next_local_index;
  env->expr_count = saved_expr_count;
  env->ctor_binding_count = saved_ctor_binding_count;
  return 0;
}

static int raw_emit_if_expr_to_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  if (!span_matches_keyword(source, start, end, "if")) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t cursor_at = skip_expr_ws(source, start + 2u, end);
  uint32_t then_at = cursor_at;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  while (then_at < end) {
    uint8_t c = mem[then_at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      then_at += 1u;
      continue;
    }
    if (c == '"') {
      in_string = 1;
      then_at += 1u;
      continue;
    }
    if (c == '(') {
      depth += 1u;
    } else if (c == ')' && depth > 0u) {
      depth -= 1u;
    } else if (depth == 0u && span_matches_keyword(source, then_at, end, "then")) {
      break;
    }
    then_at += 1u;
  }
  if (then_at >= end) {
    return 0;
  }
  uint32_t else_at = skip_expr_ws(source, then_at + 4u, end);
  uint32_t branch_split = else_at;
  depth = 0u;
  in_string = 0;
  escaped = 0;
  while (branch_split < end) {
    uint8_t c = mem[branch_split];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      branch_split += 1u;
      continue;
    }
    if (c == '"') {
      in_string = 1;
      branch_split += 1u;
      continue;
    }
    if (c == '(') {
      depth += 1u;
    } else if (c == ')' && depth > 0u) {
      depth -= 1u;
    } else if (depth == 0u && span_matches_keyword(source, branch_split, end, "else")) {
      break;
    }
    branch_split += 1u;
  }
  if (branch_split >= end) {
    return 0;
  }
  return raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      cursor_at,
      then_at,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    ) &&
    raw_emit_append_byte(out, cursor, limit, 0x04u) &&
    raw_emit_append_byte(out, cursor, limit, 0x7fu) &&
    raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      else_at,
      branch_split,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    ) &&
    raw_emit_append_byte(out, cursor, limit, 0x05u) &&
    raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      branch_split + 4u,
      end,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    ) &&
    raw_emit_append_byte(out, cursor, limit, 0x0bu);
}

static int raw_append_eq_const_condition(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t target_start,
  uint32_t target_end,
  int32_t expected,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit,
  uint32_t *cond_count,
  uint32_t inline_depth
) {
  if (
    !raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      target_start,
      target_end,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    ) ||
    !raw_emit_append_byte(out, cursor, limit, 0x41u) ||
    !raw_emit_append_var_s32(out, cursor, limit, expected) ||
    !raw_emit_append_byte(out, cursor, limit, 0x46u)
  ) {
    return 0;
  }
  if (*cond_count > 0u && !raw_emit_append_byte(out, cursor, limit, 0x71u)) {
    return 0;
  }
  *cond_count += 1u;
  return 1;
}

static int raw_emit_simple_pattern_condition(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t pattern_start,
  uint32_t pattern_end,
  uint32_t target_start,
  uint32_t target_end,
  uint32_t target_ctor_binding,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit,
  uint32_t *cond_count,
  uint32_t inline_depth
) {
  pattern_start = skip_expr_ws(source, pattern_start, pattern_end);
  pattern_end = trim_expr_end(source, pattern_start, pattern_end);
  if (pattern_start >= pattern_end) {
    return 0;
  }
  {
    uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
    if (pattern_end == pattern_start + 1u && mem[pattern_start] == '_') {
      return 1;
    }
  }
  {
    int32_t int_value = 0;
    uint32_t next = pattern_start;
    if (parse_signed_int_literal(source, pattern_start, pattern_end, &int_value, &next) &&
        next == pattern_end) {
      return raw_append_eq_const_condition(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        target_start,
        target_end,
        int_value,
        out,
        cursor,
        limit,
        cond_count,
        inline_depth + 1u
      );
    }
  }
  {
    uint32_t next = pattern_start;
    NameSpan token = parse_simple_name_token(source, pattern_start, pattern_end, &next);
    if (!token.ok || next != pattern_end) {
      return 0;
    }
    if (namespan_equals_literal(token, "true") || namespan_equals_literal(token, "True")) {
      return raw_append_eq_const_condition(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        target_start,
        target_end,
        1,
        out,
        cursor,
        limit,
        cond_count,
        inline_depth + 1u
      );
    }
    if (namespan_equals_literal(token, "false") || namespan_equals_literal(token, "False")) {
      return raw_append_eq_const_condition(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        target_start,
        target_end,
        0,
        out,
        cursor,
        limit,
        cond_count,
        inline_depth + 1u
      );
    }
    {
      uint32_t ctor_binding = target_ctor_binding;
      if (ctor_binding == MISSING_CTOR_BINDING &&
          raw_capture_constructor_binding(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            inline_depth + 1u,
            target_start,
            target_end,
            &ctor_binding,
            inline_depth + 1u
          ) &&
          ctor_binding != MISSING_CTOR_BINDING) {
        return raw_emit_bind_expr_name_with_ctor(
          env,
          token,
          target_start,
          target_end,
          ctor_binding
        );
      }
      if (ctor_binding != MISSING_CTOR_BINDING) {
        return raw_emit_bind_expr_name_with_ctor(
          env,
          token,
          target_start,
          target_end,
          ctor_binding
        );
      }
    }
    return raw_emit_bind_expr_name(
      source,
      decls,
      decl_count,
      env,
      inline_stack,
      inline_depth + 1u,
      token,
      target_start,
      target_end
    );
  }
}

static int raw_emit_case_arms_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t *target_starts,
  uint32_t *target_ends,
  uint32_t target_count,
  uint32_t arms_start,
  uint32_t end,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit,
  uint32_t inline_depth
) {
  uint32_t line_start = arms_start;
  while (line_start < end) {
    line_start = skip_expr_ws(source, line_start, end);
    if (line_start >= end) {
      return 0;
    }
    uint32_t line_end = source_line_end(source, line_start);
    if (line_end > end) {
      line_end = end;
    }
    uint32_t next_line = source_next_line_start(source, line_end);
    uint32_t cursor_at = line_start;
    uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
    if (cursor_at >= line_end ||
        (mem[cursor_at] == '-' && cursor_at + 1u < line_end && mem[cursor_at + 1u] == '-')) {
      line_start = next_line;
      continue;
    }
    uint32_t arrow_at = find_case_arm_arrow(source, cursor_at, line_end);
    if (arrow_at >= line_end) {
      return 0;
    }
    uint32_t saved_expr_count = env->expr_count;
    uint32_t saved_ctor_binding_count = env->ctor_binding_count;
    uint8_t cond_buf[512] = {0};
    uint32_t cond_at = 0u;
    uint32_t cond_count = 0u;
    uint32_t pattern_cursor = cursor_at;
    if (target_count == 1u) {
      uint32_t first_end = parse_expr_atom_end(source, pattern_cursor, arrow_at);
      if (first_end <= pattern_cursor) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      uint32_t next = pattern_cursor;
      NameSpan first = parse_simple_name_token(source, pattern_cursor, first_end, &next);
      uint32_t after_first = skip_expr_ws(source, first_end, arrow_at);
      if (first.ok && next == first_end && namespan_starts_with_upper(source, first)) {
        uint32_t ctor_arg_starts[MAX_EVAL_ARGS] = {0};
        uint32_t ctor_arg_ends[MAX_EVAL_ARGS] = {0};
        uint32_t ctor_arg_ctor_bindings[MAX_EVAL_ARGS];
        init_ctor_binding_array(ctor_arg_ctor_bindings, MAX_EVAL_ARGS);
        uint32_t ctor_arg_count = 0u;
        NameSpan ctor_name = missing_name_span();
        if (!raw_resolve_direct_constructor_target(
              source,
              decls,
              decl_count,
              env,
              inline_stack,
              inline_depth + 1u,
              target_starts[0],
              target_ends[0],
              &ctor_name,
              ctor_arg_starts,
              ctor_arg_ends,
              ctor_arg_ctor_bindings,
              MAX_EVAL_ARGS,
              &ctor_arg_count,
              0u)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        if (!names_equal(first, ctor_name)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          line_start = next_line;
          continue;
        }
        uint32_t arg_index = 0u;
        pattern_cursor = after_first;
        while (pattern_cursor < arrow_at) {
          if (arg_index >= ctor_arg_count) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          uint32_t pat_end = parse_expr_atom_end(source, pattern_cursor, arrow_at);
          if (pat_end <= pattern_cursor) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          if (!raw_emit_simple_pattern_condition(
                source,
                decls,
                decl_count,
                env,
                inline_stack,
                pattern_cursor,
                pat_end,
                ctor_arg_starts[arg_index],
                ctor_arg_ends[arg_index],
                ctor_arg_ctor_bindings[arg_index],
                cond_buf,
                &cond_at,
                sizeof(cond_buf),
                &cond_count,
                inline_depth + 1u)) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          arg_index += 1u;
          pattern_cursor = skip_expr_ws(source, pat_end, arrow_at);
        }
        if (arg_index != ctor_arg_count) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
      } else {
        if (!raw_emit_simple_pattern_condition(
              source,
              decls,
              decl_count,
              env,
              inline_stack,
              pattern_cursor,
              arrow_at,
              target_starts[0],
              target_ends[0],
              MISSING_CTOR_BINDING,
              cond_buf,
              &cond_at,
              sizeof(cond_buf),
              &cond_count,
              inline_depth + 1u)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
      }
    } else {
      for (uint32_t i = 0u; i < target_count; i += 1u) {
        pattern_cursor = skip_expr_ws(source, pattern_cursor, arrow_at);
        if (pattern_cursor >= arrow_at) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        uint32_t pat_end = parse_expr_atom_end(source, pattern_cursor, arrow_at);
        if (pat_end <= pattern_cursor) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        if (!raw_emit_simple_pattern_condition(
              source,
              decls,
              decl_count,
              env,
              inline_stack,
              pattern_cursor,
              pat_end,
              target_starts[i],
              target_ends[i],
              MISSING_CTOR_BINDING,
              cond_buf,
              &cond_at,
              sizeof(cond_buf),
              &cond_count,
              inline_depth + 1u)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        pattern_cursor = pat_end;
      }
      pattern_cursor = skip_expr_ws(source, pattern_cursor, arrow_at);
      if (pattern_cursor != arrow_at) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
    }
    if (cond_count == 0u) {
      int ok = raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arrow_at + 2u,
        line_end,
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      );
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return ok;
    }
    if (
      !raw_emit_append_bytes(out, cursor, limit, cond_buf, cond_at) ||
      !raw_emit_append_byte(out, cursor, limit, 0x04u) ||
      !raw_emit_append_byte(out, cursor, limit, 0x7fu) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        arrow_at + 2u,
        line_end,
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x05u)
    ) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
    env->expr_count = saved_expr_count;
    if (
      !raw_emit_case_arms_wasm(
        source,
        decls,
        decl_count,
        env,
        inline_stack,
        target_starts,
        target_ends,
        target_count,
        next_line,
        end,
        out,
        cursor,
        limit,
        inline_depth + 1u
      ) ||
      !raw_emit_append_byte(out, cursor, limit, 0x0bu)
    ) {
      return 0;
    }
    return 1;
  }
  return 0;
}

static int raw_emit_case_expr_to_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  if (!span_matches_keyword(source, start, end, "case")) {
    return 0;
  }
  uint32_t of_at = find_case_of_at(source, start + 4u, end);
  if (of_at >= end) {
    return 0;
  }
  uint32_t targets_start = skip_expr_ws(source, start + 4u, of_at);
  uint32_t target_starts[MAX_EVAL_ARGS];
  uint32_t target_ends[MAX_EVAL_ARGS];
  uint32_t target_count = 0u;
  uint32_t cursor_at = targets_start;
  while (cursor_at < of_at) {
    if (target_count >= MAX_EVAL_ARGS) {
      return 0;
    }
    uint32_t atom_end = parse_expr_atom_end(source, cursor_at, of_at);
    if (atom_end <= cursor_at) {
      return 0;
    }
    target_starts[target_count] = cursor_at;
    target_ends[target_count] = atom_end;
    target_count += 1u;
    cursor_at = skip_expr_ws(source, atom_end, of_at);
  }
  if (target_count == 0u) {
    return 0;
  }
  if (target_count > 1u) {
    uint32_t arms_start = skip_expr_ws(source, of_at + 2u, end);
    if (arms_start < end) {
      uint32_t line_end = source_line_end(source, arms_start);
      if (line_end > end) {
        line_end = end;
      }
      uint32_t arrow_at = find_case_arm_arrow(source, arms_start, line_end);
      if (arrow_at < line_end) {
        uint32_t first_end = parse_expr_atom_end(source, arms_start, arrow_at);
        if (first_end > arms_start) {
          uint32_t next = arms_start;
          NameSpan first = parse_simple_name_token(source, arms_start, first_end, &next);
          if (first.ok && next == first_end && namespan_starts_with_upper(source, first)) {
            target_starts[0] = targets_start;
            target_ends[0] = trim_expr_end(source, targets_start, of_at);
            target_count = 1u;
          }
        }
      }
    }
  }
  return raw_emit_case_arms_wasm(
    source,
    decls,
    decl_count,
    env,
    inline_stack,
    target_starts,
    target_ends,
    target_count,
    skip_expr_ws(source, of_at + 2u, end),
    end,
    out,
    cursor,
    limit,
    inline_depth + 1u
  );
}

static int raw_emit_expr_to_wasm(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t start,
  uint32_t end,
  RawEmitEnv *env,
  NameSpan *inline_stack,
  uint32_t inline_depth,
  uint8_t *out,
  uint32_t *cursor,
  uint32_t limit
) {
  if (inline_depth > MAX_RAW_EMIT_INLINE_DEPTH) {
    return 0;
  }
  start = skip_expr_ws(source, start, end);
  end = trim_expr_end(source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_is_wrapped_parens(source, start, end)) {
    return raw_emit_expr_to_wasm(
      source,
      decls,
      decl_count,
      start + 1u,
      end - 1u,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
  }
  if (span_matches_keyword(source, start, end, "let")) {
    int let_ok = raw_emit_let_expr_to_wasm(
      source,
      decls,
      decl_count,
      start,
      end,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
    if (let_ok) {
      return 1;
    }
  }
  if (span_matches_keyword(source, start, end, "if")) {
    int if_ok = raw_emit_if_expr_to_wasm(
      source,
      decls,
      decl_count,
      start,
      end,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
    if (if_ok) {
      return 1;
    }
  }
  if (span_matches_keyword(source, start, end, "case")) {
    int case_ok = raw_emit_case_expr_to_wasm(
      source,
      decls,
      decl_count,
      start,
      end,
      env,
      inline_stack,
      inline_depth + 1u,
      out,
      cursor,
      limit
    );
    if (case_ok) {
      return 1;
    }
  }
  for (uint32_t precedence = 0u; precedence < 5u; precedence += 1u) {
    uint32_t op_at = raw_emit_find_top_level_binary_op(source, start, end, precedence);
    if (op_at >= end) {
      continue;
    }
    uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
    uint32_t op_len = 1u;
    uint8_t opcode = 0u;
    if (op_at + 1u < end && mem[op_at + 1u] == '.') {
      op_len = 2u;
      if (mem[op_at] == '+') {
        opcode = 0x6au;
      } else if (mem[op_at] == '-') {
        opcode = 0x6bu;
      } else if (mem[op_at] == '*') {
        opcode = 0x6cu;
      } else if (mem[op_at] == '/') {
        opcode = 0x6du;
      } else if (mem[op_at] == '%') {
        opcode = 0x6fu;
      } else {
        return 0;
      }
    } else if (mem[op_at] == '&' && op_at + 1u < end && mem[op_at + 1u] == '&') {
      op_len = 2u;
      opcode = 0x71u;
    } else if (mem[op_at] == '|' && op_at + 1u < end && mem[op_at + 1u] == '|') {
      op_len = 2u;
      opcode = 0x72u;
    } else if (mem[op_at] == '=' && op_at > start && mem[op_at - 1u] == '=') {
      op_at -= 1u;
      op_len = 2u;
      opcode = 0x46u;
    } else if (mem[op_at] == '=' && op_at > start && mem[op_at - 1u] == '!') {
      op_at -= 1u;
      op_len = 2u;
      opcode = 0x47u;
    } else if (mem[op_at] == '=' && op_at > start && mem[op_at - 1u] == '<') {
      op_at -= 1u;
      op_len = 2u;
      opcode = 0x4cu;
    } else if (mem[op_at] == '=' && op_at > start && mem[op_at - 1u] == '>') {
      op_at -= 1u;
      op_len = 2u;
      opcode = 0x4eu;
    } else if (mem[op_at] == '<') {
      opcode = 0x48u;
    } else if (mem[op_at] == '>') {
      opcode = 0x4au;
    } else if (mem[op_at] == '+') {
      opcode = 0x6au;
    } else if (mem[op_at] == '-') {
      opcode = 0x6bu;
    } else if (mem[op_at] == '*') {
      opcode = 0x6cu;
    } else if (mem[op_at] == '/') {
      opcode = 0x6du;
    } else if (mem[op_at] == '%') {
      opcode = 0x6fu;
    } else {
      return 0;
    }
    uint32_t left_end = op_at;
    uint32_t right_start = op_at + op_len;
    if (
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        start,
        left_end,
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_expr_to_wasm(
        source,
        decls,
        decl_count,
        right_start,
        end,
        env,
        inline_stack,
        inline_depth + 1u,
        out,
        cursor,
        limit
      ) ||
      !raw_emit_append_byte(out, cursor, limit, opcode)
    ) {
      return 0;
    }
    return 1;
  }
  {
    uint32_t head_start = skip_expr_ws(source, start, end);
    uint32_t head_end = parse_expr_atom_end(source, head_start, end);
    if (head_end > head_start) {
      uint32_t arg_starts[MAX_EVAL_ARGS];
      uint32_t arg_ends[MAX_EVAL_ARGS];
      uint32_t argc = 0u;
      uint32_t parse_cursor = head_end;
      while (1) {
        parse_cursor = skip_expr_ws(source, parse_cursor, end);
        if (parse_cursor >= end) {
          break;
        }
        if (argc >= MAX_EVAL_ARGS) {
          return 0;
        }
        uint32_t atom_end = parse_expr_atom_end(source, parse_cursor, end);
        if (atom_end <= parse_cursor) {
          return 0;
        }
        arg_starts[argc] = parse_cursor;
        arg_ends[argc] = atom_end;
        argc += 1u;
        parse_cursor = atom_end;
      }
      if (argc == 0u) {
        return raw_emit_atom_to_wasm(
          source,
          decls,
          decl_count,
          head_start,
          head_end,
          env,
          inline_stack,
          inline_depth + 1u,
          out,
          cursor,
          limit
        );
      }
      if (span_is_wrapped_parens(source, head_start, head_end)) {
        if (raw_emit_lambda_apply_wasm(
              source,
              decls,
              decl_count,
              env,
              inline_stack,
              head_start + 1u,
              head_end - 1u,
              arg_starts,
              arg_ends,
              argc,
              out,
              cursor,
              limit,
              inline_depth + 1u
            )) {
          return 1;
        }
      }
      {
        uint32_t next = head_start;
        NameSpan head = parse_simple_name_token(source, head_start, head_end, &next);
        if (!head.ok || next != head_end) {
          return raw_emit_bound_apply_wasm(
            source,
            decls,
            decl_count,
            env,
            inline_stack,
            head_start,
            head_end,
            arg_starts,
            arg_ends,
            argc,
            out,
            cursor,
            limit,
            inline_depth + 1u
          );
        }
        {
          uint32_t bound_start = 0u;
          uint32_t bound_end = 0u;
          if (raw_emit_lookup_expr_binding(head, env, &bound_start, &bound_end, NULL)) {
            uint32_t resolved_start = bound_start;
            uint32_t resolved_end = bound_end;
            if (raw_resolve_bound_expr_span(
                  source,
                  env,
                  bound_start,
                  bound_end,
                  &resolved_start,
                  &resolved_end,
                  0u
                ) &&
                raw_emit_lambda_apply_wasm(
                  source,
                  decls,
                  decl_count,
                  env,
                  inline_stack,
                  resolved_start,
                  resolved_end,
                  arg_starts,
                  arg_ends,
                  argc,
                  out,
                  cursor,
                  limit,
                  inline_depth + 1u
                )) {
              return 1;
            }
          }
        }
        {
          int decl_index = find_decl_index_by_name(decls, decl_count, head);
          if (decl_index >= 0 &&
              decl_param_count(source, decls[(uint32_t) decl_index]) == 0u) {
            FnDecl decl = decls[(uint32_t) decl_index];
            uint32_t decl_end = decl.body_end > decl.body_start
              ? decl.body_end
              : decl_expression_end(source, decls, decl_count, (uint32_t) decl_index);
            if (raw_emit_lambda_apply_wasm(
                  source,
                  decls,
                  decl_count,
                  env,
                  inline_stack,
                  decl.body_start,
                  decl_end,
                  arg_starts,
                  arg_ends,
                  argc,
                  out,
                  cursor,
                  limit,
                  inline_depth + 1u
                )) {
              return 1;
            }
          }
        }
        if (
          raw_emit_apply_named_builtin(
            source,
            decls,
            decl_count,
            head,
            arg_starts,
            arg_ends,
            argc,
            env,
            inline_stack,
            inline_depth + 1u,
            out,
            cursor,
            limit
          )
        ) {
          return 1;
        }
        if (raw_emit_inline_named_decl(
              source,
              decls,
              decl_count,
              head,
              arg_starts,
              arg_ends,
              argc,
              env,
              inline_stack,
              inline_depth + 1u,
              out,
              cursor,
              limit
            )) {
          return 1;
        }
        {
          int decl_index = find_decl_index_by_name(decls, decl_count, head);
          if (decl_index >= 0 &&
              env != NULL &&
              env->function_index_by_decl != NULL &&
              env->function_index_by_decl[decl_index] >= 0 &&
              decl_param_count(source, decls[(uint32_t) decl_index]) == argc) {
            for (uint32_t i = 0u; i < argc; i += 1u) {
              if (!raw_emit_expr_to_wasm(
                    source,
                    decls,
                    decl_count,
                    arg_starts[i],
                    arg_ends[i],
                    env,
                    inline_stack,
                    inline_depth + 1u,
                    out,
                    cursor,
                    limit
                  )) {
                return 0;
              }
            }
            return raw_emit_append_byte(out, cursor, limit, 0x10u) &&
              raw_emit_append_var_u32(out, cursor, limit, (uint32_t) env->function_index_by_decl[decl_index]);
          }
        }
        return raw_emit_bound_apply_wasm(
          source,
          decls,
          decl_count,
          env,
          inline_stack,
          head_start,
          head_end,
          arg_starts,
          arg_ends,
          argc,
          out,
          cursor,
          limit,
          inline_depth + 1u
        );
      }
    }
  }
  return 0;
}

static uint32_t encode_var_s32_bytes(int32_t value, uint8_t *out) {
  uint32_t cursor = 0u;
  int32_t n = value;
  while (1) {
    uint8_t byte = (uint8_t) (n & 0x7f);
    int32_t sign = byte & 0x40u;
    n >>= 7;
    if ((n == 0 && sign == 0) || (n == -1 && sign != 0)) {
      out[cursor++] = byte;
      return cursor;
    }
    out[cursor++] = (uint8_t) (byte | 0x80u);
  }
}

#define MAX_PHASE1_EMIT_LOCALS 64u
#define MAX_PHASE1_EMIT_TYPES 16u
#define MAX_PHASE1_EMIT_EXPR_BINDINGS 64u
#define MAX_PHASE1_EMIT_INLINE_DEPTH 8u
#define MAX_PHASE1_EMIT_CTOR_BINDINGS 64u

typedef struct {
  NameSpan ctor_name;
  uint32_t expr_start;
  uint32_t expr_end;
  uint32_t arg_starts[MAX_EVAL_ARGS];
  uint32_t arg_ends[MAX_EVAL_ARGS];
  uint32_t arg_ctor_bindings[MAX_EVAL_ARGS];
  uint32_t arg_count;
} Phase1CtorBinding;

typedef struct {
  Segment source;
  FnDecl *decls;
  uint32_t decl_count;
  int *function_index_by_decl;
  NameSpan local_names[MAX_PHASE1_EMIT_LOCALS];
  uint32_t local_indices[MAX_PHASE1_EMIT_LOCALS];
  NameSpan expr_names[MAX_PHASE1_EMIT_EXPR_BINDINGS];
  uint32_t expr_starts[MAX_PHASE1_EMIT_EXPR_BINDINGS];
  uint32_t expr_ends[MAX_PHASE1_EMIT_EXPR_BINDINGS];
  uint32_t expr_ctor_bindings[MAX_PHASE1_EMIT_EXPR_BINDINGS];
  uint32_t expr_count;
  NameSpan inline_stack[MAX_PHASE1_EMIT_INLINE_DEPTH];
  uint32_t inline_count;
  Phase1CtorBinding ctor_bindings[MAX_PHASE1_EMIT_CTOR_BINDINGS];
  uint32_t ctor_binding_count;
  uint32_t local_count;
  uint32_t param_count;
  uint32_t next_local_index;
} Phase1EmitEnv;

static int roots_have_unknown_names(FnDecl *decls, uint32_t decl_count, NameSpan *roots, uint32_t roots_count);
static void seed_reachable(FnDecl *decls, uint32_t decl_count, NameSpan *roots, uint32_t roots_count, int *reachable);
static void expand_reachable(Segment source, FnDecl *decls, uint32_t decl_count, int *reachable);
static int decl_inline_only_candidate(Segment source, FnDecl *decls, uint32_t decl_count, uint32_t decl_index);
static int bind_emit_expr_name(
  Phase1EmitEnv *env,
  NameSpan name,
  uint32_t expr_start,
  uint32_t expr_end
);
static int bind_emit_expr_name_with_ctor(
  Phase1EmitEnv *env,
  NameSpan name,
  uint32_t expr_start,
  uint32_t expr_end,
  uint32_t ctor_binding
);
static int capture_constructor_binding(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *binding_index_out,
  uint32_t depth
);

static void normalize_ctor_binding(
  Phase1EmitEnv *env,
  uint32_t binding_index,
  uint32_t expr_start,
  uint32_t expr_end,
  uint32_t depth
);

static int ensure_type_for_arity(
  uint32_t arity,
  int *type_by_arity,
  uint32_t *arity_list,
  uint32_t *arity_count,
  uint32_t *type_index_out
) {
  if (arity >= MAX_PHASE1_EMIT_TYPES) {
    return 0;
  }
  if (type_by_arity[arity] >= 0) {
    *type_index_out = (uint32_t) type_by_arity[arity];
    return 1;
  }
  type_by_arity[arity] = (int) *arity_count;
  arity_list[*arity_count] = arity;
  *type_index_out = *arity_count;
  *arity_count += 1u;
  return 1;
}

static int decl_inline_only_candidate(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  uint32_t decl_index
) {
  FnDecl decl = decls[decl_index];
  uint32_t function_end = decl_function_end(decls, decl_count, decl_index, source.len);
  uint32_t expr_end = decl_expression_end(source, decls, decl_count, decl_index);
  if (expr_end <= decl.body_start) {
    expr_end = function_end;
  }
  uint32_t start = skip_expr_ws(source, decl.body_start, expr_end);
  if (start >= expr_end) {
    return 0;
  }
  if (decl_param_count(source, decl) == 0u) {
    NameSpan head = missing_name_span();
    uint32_t arg_starts[MAX_EVAL_ARGS] = {0};
    uint32_t arg_ends[MAX_EVAL_ARGS] = {0};
    uint32_t argc = 0u;
    if (raw_parse_apply_span(source, start, expr_end, &head, arg_starts, arg_ends, MAX_EVAL_ARGS, &argc) &&
        argc > 0u && !namespan_starts_with_upper(source, head)) {
      return 1;
    }
  }
  {
    NameSpan lambda_params[MAX_EVAL_ARGS];
    uint32_t lambda_param_count = 0u;
    uint32_t lambda_body_start = 0u;
    if (parse_lambda_expr(
          source,
          start,
          expr_end,
          lambda_params,
          MAX_EVAL_ARGS,
          &lambda_param_count,
          &lambda_body_start)) {
      return 1;
    }
  }
  {
    NameSpan params[MAX_EVAL_ARGS];
    uint32_t param_count = collect_decl_params(source, decl, params, MAX_EVAL_ARGS);
    if (param_count > 0u) {
      NameSpan head = missing_name_span();
      uint32_t arg_starts[MAX_EVAL_ARGS];
      uint32_t arg_ends[MAX_EVAL_ARGS];
      uint32_t argc = 0u;
      if (raw_parse_apply_span(
            source,
            start,
            expr_end,
            &head,
            arg_starts,
            arg_ends,
            MAX_EVAL_ARGS,
            &argc) &&
          argc > 0u) {
        int all_args_are_params = argc > 0u;
        for (uint32_t arg = 0u; arg < argc; arg += 1u) {
          int matched_param = 0;
          for (uint32_t i = 0u; i < param_count; i += 1u) {
            if (span_is_exact_simple_name(source, arg_starts[arg], arg_ends[arg], params[i])) {
              matched_param = 1;
              break;
            }
          }
          if (!matched_param) {
            all_args_are_params = 0;
            break;
          }
        }
        if (all_args_are_params) {
          return 1;
        }
        for (uint32_t i = 0u; i < param_count; i += 1u) {
          if (names_equal(params[i], head)) {
            return 1;
          }
        }
      }
    }
  }
  {
    uint32_t next = start;
    NameSpan first = parse_simple_name_token(source, start, expr_end, &next);
    if (first.ok && next <= expr_end && namespan_starts_with_upper(source, first)) {
      return 1;
    }
  }
  if (span_matches_keyword(source, start, expr_end, "case")) {
    return 1;
  }
  if (span_matches_keyword(source, start, expr_end, "let")) {
    uint32_t cursor = skip_expr_ws(source, start + 3u, expr_end);
    uint32_t next = cursor;
    NameSpan first = parse_simple_name_token(source, cursor, expr_end, &next);
    if (first.ok && namespan_starts_with_upper(source, first)) {
      return 1;
    }
    while (cursor < expr_end) {
      cursor = skip_expr_ws(source, cursor, expr_end);
      if (cursor >= expr_end) {
        break;
      }
      if (span_matches_keyword(source, cursor, expr_end, "in")) {
        uint32_t body_start = skip_expr_ws(source, cursor + 2u, expr_end);
        uint32_t body_next = body_start;
        NameSpan body_head = parse_simple_name_token(source, body_start, expr_end, &body_next);
        if (body_head.ok && body_next <= expr_end && namespan_starts_with_upper(source, body_head)) {
          return 1;
        }
        break;
      }
      uint32_t line_end = source_line_end(source, cursor);
      if (line_end > expr_end) {
        line_end = expr_end;
      }
      cursor = source_next_line_start(source, line_end);
    }
  }
  {
    RawEmitEnv env = {0};
    NameSpan inline_stack[MAX_RAW_EMIT_INLINE_DEPTH] = {0};
    NameSpan params[MAX_EVAL_ARGS] = {0};
    uint32_t param_count = collect_decl_params(source, decl, params, MAX_EVAL_ARGS);
    if (param_count <= MAX_EVAL_LOCALS) {
      env.count = 0u;
      env.next_local_index = param_count;
      env.function_index_by_decl = NULL;
      for (uint32_t i = 0u; i < param_count; i += 1u) {
        env.names[env.count] = params[i];
        env.indices[env.count] = i;
        env.count += 1u;
      }
      NameSpan ctor_name = missing_name_span();
      uint32_t arg_starts[MAX_EVAL_ARGS] = {0};
      uint32_t arg_ends[MAX_EVAL_ARGS] = {0};
      uint32_t arg_ctor_bindings[MAX_EVAL_ARGS];
      init_ctor_binding_array(arg_ctor_bindings, MAX_EVAL_ARGS);
      uint32_t argc = 0u;
      if (raw_resolve_direct_constructor_target(
            source,
            decls,
            decl_count,
            &env,
            inline_stack,
            0u,
            decl.body_start,
            expr_end,
            &ctor_name,
            arg_starts,
            arg_ends,
            arg_ctor_bindings,
            MAX_EVAL_ARGS,
            &argc,
            0u)) {
        return 1;
      }
    }
  }
  return 0;
}

static int append_buf_u8(uint8_t *buf, uint32_t cap, uint32_t *at, uint8_t byte) {
  if (*at >= cap) {
    return 0;
  }
  buf[*at] = byte;
  *at += 1u;
  return 1;
}

static int append_buf_var_u32(uint8_t *buf, uint32_t cap, uint32_t *at, uint32_t value) {
  uint8_t tmp[5];
  uint32_t len = append_var_u32(tmp, 0u, value);
  if (*at + len > cap) {
    return 0;
  }
  for (uint32_t i = 0u; i < len; i += 1u) {
    buf[*at] = tmp[i];
    *at += 1u;
  }
  return 1;
}

static int append_buf_var_s32(uint8_t *buf, uint32_t cap, uint32_t *at, int32_t value) {
  uint8_t tmp[5];
  uint32_t len = encode_var_s32_bytes(value, tmp);
  if (*at + len > cap) {
    return 0;
  }
  for (uint32_t i = 0u; i < len; i += 1u) {
    buf[*at] = tmp[i];
    *at += 1u;
  }
  return 1;
}

static int append_i32_const_instr(uint8_t *buf, uint32_t cap, uint32_t *at, int32_t value) {
  return append_buf_u8(buf, cap, at, 0x41u) &&
    append_buf_var_s32(buf, cap, at, value);
}

static int append_local_get_instr(uint8_t *buf, uint32_t cap, uint32_t *at, uint32_t local_index) {
  return append_buf_u8(buf, cap, at, 0x20u) &&
    append_buf_var_u32(buf, cap, at, local_index);
}

static int append_local_set_instr(uint8_t *buf, uint32_t cap, uint32_t *at, uint32_t local_index) {
  return append_buf_u8(buf, cap, at, 0x21u) &&
    append_buf_var_u32(buf, cap, at, local_index);
}

static int append_call_instr(uint8_t *buf, uint32_t cap, uint32_t *at, uint32_t function_index) {
  return append_buf_u8(buf, cap, at, 0x10u) &&
    append_buf_var_u32(buf, cap, at, function_index);
}

static int is_name_token_boundary_before(Segment source, uint32_t at, uint32_t start) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (at > start && is_ident_continue(mem[at - 1u])) {
    return 0;
  }
  return 1;
}

static int is_name_token_boundary_after(Segment source, uint32_t at, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  if (at < end && is_ident_continue(mem[at])) {
    return 0;
  }
  return 1;
}

static int match_keyword_at(Segment source, uint32_t at, uint32_t start, uint32_t end, const char *keyword) {
  uint32_t len = cstr_len(keyword);
  if (at + len > end) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  for (uint32_t i = 0u; i < len; i += 1u) {
    if (mem[at + i] != (uint8_t) keyword[i]) {
      return 0;
    }
  }
  return is_name_token_boundary_before(source, at, start) &&
    is_name_token_boundary_after(source, at + len, end);
}

static int lookup_emit_local_index(Phase1EmitEnv *env, NameSpan name, uint32_t *out_index) {
  for (uint32_t i = env->local_count; i > 0u; i -= 1u) {
    uint32_t at = i - 1u;
    if (names_equal(name, env->local_names[at])) {
      *out_index = env->local_indices[at];
      return 1;
    }
  }
  return 0;
}

static int lookup_emit_expr_binding(
  Phase1EmitEnv *env,
  NameSpan name,
  uint32_t *out_start,
  uint32_t *out_end,
  uint32_t *out_ctor_binding
) {
  for (uint32_t i = env->expr_count; i > 0u; i -= 1u) {
    uint32_t at = i - 1u;
    if (names_equal(name, env->expr_names[at])) {
      *out_start = env->expr_starts[at];
      *out_end = env->expr_ends[at];
      if (out_ctor_binding) {
        *out_ctor_binding = env->expr_ctor_bindings[at];
      }
      return 1;
    }
  }
  return 0;
}

static int emit_inline_stack_has(Phase1EmitEnv *env, NameSpan name) {
  for (uint32_t i = 0u; i < env->inline_count; i += 1u) {
    if (names_equal(env->inline_stack[i], name)) {
      return 1;
    }
  }
  return 0;
}

static int resolve_bound_expr_span(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *out_start,
  uint32_t *out_end,
  uint32_t depth
) {
  if (depth > 16u) {
    return 0;
  }
  start = skip_expr_ws(env->source, start, end);
  end = trim_expr_end(env->source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_is_wrapped_parens(env->source, start, end)) {
    return resolve_bound_expr_span(env, start + 1u, end - 1u, out_start, out_end, depth + 1u);
  }
  {
    uint32_t next = start;
    NameSpan name = parse_simple_name_token(env->source, start, end, &next);
    if (name.ok && next == end) {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      if (lookup_emit_expr_binding(env, name, &bound_start, &bound_end, NULL)) {
        return resolve_bound_expr_span(env, bound_start, bound_end, out_start, out_end, depth + 1u);
      }
    }
  }
  *out_start = start;
  *out_end = end;
  return 1;
}

static int parse_apply_span(
  Segment source,
  uint32_t start,
  uint32_t end,
  NameSpan *head_out,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t max_args,
  uint32_t *argc_out
) {
  uint32_t head_start = skip_expr_ws(source, start, end);
  uint32_t head_end = parse_expr_atom_end(source, head_start, end);
  if (head_end <= head_start) {
    return 0;
  }
  uint32_t next = head_start;
  NameSpan head = parse_simple_name_token(source, head_start, head_end, &next);
  if (!head.ok || next != head_end) {
    return 0;
  }
  uint32_t argc = 0u;
  uint32_t cursor = skip_expr_ws(source, head_end, end);
  while (cursor < end) {
    if (argc >= max_args) {
      return 0;
    }
    uint32_t atom_end = parse_expr_atom_end(source, cursor, end);
    if (atom_end <= cursor) {
      return 0;
    }
    arg_starts[argc] = cursor;
    arg_ends[argc] = atom_end;
    argc += 1u;
    cursor = skip_expr_ws(source, atom_end, end);
  }
  *head_out = head;
  *argc_out = argc;
  return 1;
}

static int namespan_starts_with_upper(Segment source, NameSpan name) {
  if (!name.ok || name.len == 0u || name.ptr < source.ptr) {
    return 0;
  }
  uint32_t offset = name.ptr - source.ptr;
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint8_t c = mem[offset];
  return c >= 'A' && c <= 'Z';
}

static int namespan_has_prefix(Segment source, NameSpan name, const char *prefix) {
  if (!name.ok) {
    return 0;
  }
  uint32_t prefix_len = cstr_len(prefix);
  if (name.len < prefix_len || name.ptr < source.ptr) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t offset = name.ptr - source.ptr;
  for (uint32_t i = 0u; i < prefix_len; i += 1u) {
    if (mem[offset + i] != (uint8_t) prefix[i]) {
      return 0;
    }
  }
  return 1;
}

static int parse_helper_trailing_u32(Segment source, NameSpan name, uint32_t start_at, uint32_t *out_value) {
  if (!name.ok || name.ptr < source.ptr) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t offset = name.ptr - source.ptr;
  if (start_at >= name.len) {
    return 0;
  }
  uint32_t value = 0u;
  for (uint32_t i = start_at; i < name.len; i += 1u) {
    uint8_t c = mem[offset + i];
    if (c < '0' || c > '9') {
      return 0;
    }
    value = value * 10u + (uint32_t) (c - '0');
  }
  *out_value = value;
  return 1;
}

static int helper_tags_equal(Segment lhs_source, NameSpan lhs, Segment rhs_source, NameSpan rhs) {
  if (lhs.len != rhs.len || lhs.ptr < lhs_source.ptr || rhs.ptr < rhs_source.ptr) {
    return 0;
  }
  uint8_t *lhs_mem = (uint8_t *) (uintptr_t) lhs_source.ptr;
  uint8_t *rhs_mem = (uint8_t *) (uintptr_t) rhs_source.ptr;
  uint32_t lhs_offset = lhs.ptr - lhs_source.ptr;
  uint32_t rhs_offset = rhs.ptr - rhs_source.ptr;
  for (uint32_t i = 0u; i < lhs.len; i += 1u) {
    if (lhs_mem[lhs_offset + i] != rhs_mem[rhs_offset + i]) {
      return 0;
    }
  }
  return 1;
}

static int parse_mk_helper_name(
  Segment source,
  NameSpan name,
  NameSpan *tag_out,
  uint32_t *arity_out
) {
  if (!namespan_has_prefix(source, name, "__mk_") || name.ptr < source.ptr) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t offset = name.ptr - source.ptr;
  uint32_t tag_start = 5u;
  for (uint32_t i = tag_start; i < name.len; i += 1u) {
    if (mem[offset + i] == '_') {
      NameSpan tag;
      tag.ptr = name.ptr + tag_start;
      tag.len = i - tag_start;
      tag.ok = tag.len > 0u;
      if (!tag.ok || !parse_helper_trailing_u32(source, name, i + 1u, arity_out)) {
        return 0;
      }
      *tag_out = tag;
      return 1;
    }
  }
  return 0;
}

static int parse_get_helper_name(
  Segment source,
  NameSpan name,
  NameSpan *tag_out,
  uint32_t *index_out
) {
  if (!namespan_has_prefix(source, name, "__get_") || name.ptr < source.ptr) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t offset = name.ptr - source.ptr;
  uint32_t tag_start = 6u;
  for (uint32_t i = tag_start; i < name.len; i += 1u) {
    if (mem[offset + i] == '_') {
      NameSpan tag;
      tag.ptr = name.ptr + tag_start;
      tag.len = i - tag_start;
      tag.ok = tag.len > 0u;
      if (!tag.ok || !parse_helper_trailing_u32(source, name, i + 1u, index_out)) {
        return 0;
      }
      *tag_out = tag;
      return 1;
    }
  }
  return 0;
}

static int parse_is_helper_name(
  Segment source,
  NameSpan name,
  NameSpan *tag_out
) {
  if (!namespan_has_prefix(source, name, "__is_")) {
    return 0;
  }
  NameSpan tag;
  tag.ptr = name.ptr + 5u;
  tag.len = name.len - 5u;
  tag.ok = tag.len > 0u;
  if (!tag.ok) {
    return 0;
  }
  *tag_out = tag;
  return 1;
}

static int resolve_direct_constructor_target(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  NameSpan *ctor_name_out,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t *arg_ctor_bindings,
  uint32_t max_args,
  uint32_t *argc_out,
  uint32_t depth
) {
  if (depth > MAX_PHASE1_EMIT_INLINE_DEPTH) {
    return 0;
  }
  {
    uint32_t simple_start = skip_expr_ws(env->source, start, end);
    uint32_t simple_end = trim_expr_end(env->source, simple_start, end);
    if (span_is_wrapped_parens(env->source, simple_start, simple_end)) {
      simple_start += 1u;
      simple_end -= 1u;
    }
    if (simple_start < simple_end) {
      uint32_t next = simple_start;
      NameSpan simple = parse_simple_name_token(env->source, simple_start, simple_end, &next);
      if (simple.ok && next == simple_end) {
        uint32_t bound_start = 0u;
        uint32_t bound_end = 0u;
        uint32_t ctor_binding = MISSING_CTOR_BINDING;
        if (lookup_emit_expr_binding(env, simple, &bound_start, &bound_end, &ctor_binding) &&
            ctor_binding != MISSING_CTOR_BINDING &&
            ctor_binding < env->ctor_binding_count) {
          Phase1CtorBinding *binding = &env->ctor_bindings[ctor_binding];
          if (binding->arg_count > max_args) {
            return 0;
          }
          *ctor_name_out = binding->ctor_name;
          *argc_out = binding->arg_count;
          for (uint32_t i = 0u; i < binding->arg_count; i += 1u) {
            arg_starts[i] = binding->arg_starts[i];
            arg_ends[i] = binding->arg_ends[i];
            if (arg_ctor_bindings) {
              arg_ctor_bindings[i] = binding->arg_ctor_bindings[i];
            }
          }
          return 1;
        }
      }
    }
  }
  if (!resolve_bound_expr_span(env, start, end, &start, &end, 0u)) {
    return 0;
  }
  if (span_matches_keyword(env->source, start, end, "case")) {
    uint32_t of_at = find_case_of_at(env->source, start + 4u, end);
    if (of_at >= end) {
      return 0;
    }
    uint32_t target_start = skip_expr_ws(env->source, start + 4u, of_at);
    uint32_t target_end = trim_expr_end(env->source, target_start, of_at);
    if (target_start >= target_end) {
      return 0;
    }
    uint32_t line_start = skip_expr_ws(env->source, of_at + 2u, end);
    while (line_start < end) {
      uint32_t line_end = source_line_end(env->source, line_start);
      if (line_end > end) {
        line_end = end;
      }
      uint32_t next_line = source_next_line_start(env->source, line_end);
      uint8_t *mem = (uint8_t *) (uintptr_t) env->source.ptr;
      if (line_start >= line_end ||
          (mem[line_start] == '-' && line_start + 1u < line_end && mem[line_start + 1u] == '-')) {
        line_start = skip_expr_ws(env->source, next_line, end);
        continue;
      }
      uint32_t arrow_at = find_case_arm_arrow(env->source, line_start, line_end);
      if (arrow_at >= line_end) {
        return 0;
      }
      uint32_t saved_expr_count = env->expr_count;
      uint32_t saved_ctor_binding_count = env->ctor_binding_count;
      uint32_t pat_start = line_start;
      uint32_t first_end = parse_expr_atom_end(env->source, pat_start, arrow_at);
      if (first_end <= pat_start) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      uint32_t next = pat_start;
      NameSpan first = parse_simple_name_token(env->source, pat_start, first_end, &next);
      int matched = 0;
      if (first.ok && next == first_end && namespan_starts_with_upper(env->source, first)) {
        NameSpan target_ctor = missing_name_span();
        uint32_t target_arg_starts[MAX_EVAL_ARGS] = {0};
        uint32_t target_arg_ends[MAX_EVAL_ARGS] = {0};
        uint32_t target_arg_ctor_bindings[MAX_EVAL_ARGS];
        init_ctor_binding_array(target_arg_ctor_bindings, MAX_EVAL_ARGS);
        uint32_t target_arg_count = 0u;
        if (!resolve_direct_constructor_target(
              env,
              target_start,
              target_end,
              &target_ctor,
              target_arg_starts,
              target_arg_ends,
              target_arg_ctor_bindings,
              MAX_EVAL_ARGS,
              &target_arg_count,
              depth + 1u)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        if (!names_equal(first, target_ctor)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          line_start = skip_expr_ws(env->source, next_line, end);
          continue;
        }
        uint32_t pattern_cursor = skip_expr_ws(env->source, first_end, arrow_at);
        uint32_t arg_index = 0u;
        while (pattern_cursor < arrow_at) {
          if (arg_index >= target_arg_count) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          uint32_t pat_end = parse_expr_atom_end(env->source, pattern_cursor, arrow_at);
          if (pat_end <= pattern_cursor) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          if (!(pat_end == pattern_cursor + 1u && mem[pattern_cursor] == '_')) {
            uint32_t pat_next = pattern_cursor;
            NameSpan pat_name = parse_simple_name_token(env->source, pattern_cursor, pat_end, &pat_next);
            int bind_ok = 0;
            if (pat_name.ok && pat_next == pat_end) {
              if (target_arg_ctor_bindings[arg_index] != MISSING_CTOR_BINDING) {
                bind_ok = bind_emit_expr_name_with_ctor(
                  env,
                  pat_name,
                  target_arg_starts[arg_index],
                  target_arg_ends[arg_index],
                  target_arg_ctor_bindings[arg_index]
                );
              } else {
                bind_ok = bind_emit_expr_name(
                  env,
                  pat_name,
                  target_arg_starts[arg_index],
                  target_arg_ends[arg_index]
                );
              }
            }
            if (!bind_ok) {
              env->expr_count = saved_expr_count;
              env->ctor_binding_count = saved_ctor_binding_count;
              return 0;
            }
          }
          arg_index += 1u;
          pattern_cursor = skip_expr_ws(env->source, pat_end, arrow_at);
        }
        if (arg_index != target_arg_count) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        matched = 1;
      } else if (first_end == pat_start + 1u && mem[pat_start] == '_') {
        matched = 1;
      } else if (first.ok && next == arrow_at) {
        if (!bind_emit_expr_name(env, first, target_start, target_end)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        matched = 1;
      } else {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      if (matched) {
        int ok = resolve_direct_constructor_target(
          env,
          arrow_at + 2u,
          line_end,
          ctor_name_out,
          arg_starts,
          arg_ends,
          arg_ctor_bindings,
          max_args,
          argc_out,
          depth + 1u
        );
        if (ok) {
          for (uint32_t i = 0u; i < *argc_out; i += 1u) {
            uint32_t resolved_start = 0u;
            uint32_t resolved_end = 0u;
            if (resolve_bound_expr_span(env, arg_starts[i], arg_ends[i], &resolved_start, &resolved_end, 0u)) {
              arg_starts[i] = resolved_start;
              arg_ends[i] = resolved_end;
            }
            if (arg_ctor_bindings &&
                capture_constructor_binding(env, arg_starts[i], arg_ends[i], &arg_ctor_bindings[i], depth + 1u) &&
                arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
              normalize_ctor_binding(
                env,
                arg_ctor_bindings[i],
                arg_starts[i],
                arg_ends[i],
                depth + 1u
              );
              Phase1CtorBinding *binding = &env->ctor_bindings[arg_ctor_bindings[i]];
              arg_starts[i] = binding->expr_start;
              arg_ends[i] = binding->expr_end;
            }
          }
        }
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return ok;
      }
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      line_start = skip_expr_ws(env->source, next_line, end);
    }
    return 0;
  }
  if (span_matches_keyword(env->source, start, end, "let")) {
    uint8_t *mem = (uint8_t *) (uintptr_t) env->source.ptr;
    uint32_t cursor = start + 3u;
    uint32_t saved_expr_count = env->expr_count;
    uint32_t saved_ctor_binding_count = env->ctor_binding_count;
    while (cursor < end) {
      cursor = skip_expr_ws(env->source, cursor, end);
      if (cursor >= end) {
        break;
      }
      if (span_matches_keyword(env->source, cursor, end, "in")) {
        uint32_t in_line_end = source_line_end(env->source, cursor);
        if (in_line_end > end) {
          in_line_end = end;
        }
        int ok = resolve_direct_constructor_target(
          env,
          cursor + 2u,
          in_line_end,
          ctor_name_out,
          arg_starts,
          arg_ends,
          arg_ctor_bindings,
          max_args,
          argc_out,
          depth + 1u
        );
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return ok;
      }
      uint32_t name_next = cursor;
      NameSpan name = parse_simple_name_token(env->source, cursor, end, &name_next);
      uint32_t line_end = source_line_end(env->source, cursor);
      if (!name.ok || line_end > end) {
        if (line_end > end) {
          line_end = end;
        } else {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
      }
      uint32_t eq_at = skip_expr_ws(env->source, name_next, line_end);
      if (eq_at >= line_end || mem[eq_at] != '=') {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      uint32_t value_end = line_end;
      uint32_t next_cursor = line_end;
      int found_in = 0;
      if (!find_let_binding_split(env->source, eq_at + 1u, end, line_end, &value_end, &next_cursor, &found_in) ||
          !bind_emit_expr_name(env, name, eq_at + 1u, value_end)) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      if (found_in) {
        int ok = resolve_direct_constructor_target(
          env,
          next_cursor,
          end,
          ctor_name_out,
          arg_starts,
          arg_ends,
          arg_ctor_bindings,
          max_args,
          argc_out,
          depth + 1u
        );
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return ok;
      }
      cursor = next_cursor;
    }
    env->expr_count = saved_expr_count;
    env->ctor_binding_count = saved_ctor_binding_count;
    return 0;
  }
  NameSpan head = missing_name_span();
  uint32_t argc = 0u;
  if (!parse_apply_span(
        env->source,
        start,
        end,
        &head,
        arg_starts,
        arg_ends,
        max_args,
        &argc)) {
    return 0;
  }
  if (namespan_starts_with_upper(env->source, head)) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      uint32_t resolved_start = 0u;
      uint32_t resolved_end = 0u;
      if (resolve_bound_expr_span(env, arg_starts[i], arg_ends[i], &resolved_start, &resolved_end, 0u)) {
        arg_starts[i] = resolved_start;
        arg_ends[i] = resolved_end;
      }
      if (arg_ctor_bindings &&
          capture_constructor_binding(env, arg_starts[i], arg_ends[i], &arg_ctor_bindings[i], depth + 1u) &&
          arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
        normalize_ctor_binding(
          env,
          arg_ctor_bindings[i],
          arg_starts[i],
          arg_ends[i],
          depth + 1u
        );
        Phase1CtorBinding *binding = &env->ctor_bindings[arg_ctor_bindings[i]];
        arg_starts[i] = binding->expr_start;
        arg_ends[i] = binding->expr_end;
      }
    }
    *ctor_name_out = head;
    *argc_out = argc;
    return 1;
  }
  if (emit_inline_stack_has(env, head)) {
    return 0;
  }
  int decl_index = find_decl_index_by_name(env->decls, env->decl_count, head);
  if (decl_index < 0) {
    return 0;
  }
  FnDecl decl = env->decls[(uint32_t) decl_index];
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = collect_decl_params(env->source, decl, params, MAX_EVAL_ARGS);
  if (param_count != argc || env->expr_count + argc > MAX_PHASE1_EMIT_EXPR_BINDINGS ||
      env->inline_count >= MAX_PHASE1_EMIT_INLINE_DEPTH) {
    return 0;
  }
  uint32_t saved_expr_count = env->expr_count;
  uint32_t saved_ctor_binding_count = env->ctor_binding_count;
  uint32_t saved_inline_count = env->inline_count;
  for (uint32_t i = 0u; i < argc; i += 1u) {
    uint32_t original_next = arg_starts[i];
    uint32_t original_ctor_binding = MISSING_CTOR_BINDING;
    uint32_t original_bound_start = 0u;
    uint32_t original_bound_end = 0u;
    NameSpan original_name = parse_simple_name_token(env->source, arg_starts[i], arg_ends[i], &original_next);
    if (original_name.ok && original_next == arg_ends[i]) {
      lookup_emit_expr_binding(
        env,
        original_name,
        &original_bound_start,
        &original_bound_end,
        &original_ctor_binding
      );
    }
    uint32_t resolved_start = arg_starts[i];
    uint32_t resolved_end = arg_ends[i];
    resolve_bound_expr_span(
      env,
      arg_starts[i],
      arg_ends[i],
      &resolved_start,
      &resolved_end,
      0u
    );
    if (original_ctor_binding != MISSING_CTOR_BINDING) {
      if (!bind_emit_expr_name_with_ctor(
            env,
            params[i],
            original_bound_start,
            original_bound_end,
            original_ctor_binding)) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        env->inline_count = saved_inline_count;
        return 0;
      }
      continue;
    }
    {
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (capture_constructor_binding(env, arg_starts[i], arg_ends[i], &ctor_binding, depth + 1u)) {
        if (!bind_emit_expr_name_with_ctor(
              env,
              params[i],
              resolved_start,
              resolved_end,
              ctor_binding)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          env->inline_count = saved_inline_count;
          return 0;
        }
        continue;
      }
    }
    if (!bind_emit_expr_name(env, params[i], resolved_start, resolved_end)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      env->inline_count = saved_inline_count;
      return 0;
    }
  }
  env->inline_stack[env->inline_count] = head;
  env->inline_count += 1u;
  uint32_t function_end = decl_function_end(env->decls, env->decl_count, (uint32_t) decl_index, env->source.len);
  uint32_t expr_end = decl_expression_end(env->source, env->decls, env->decl_count, (uint32_t) decl_index);
  int ok = resolve_direct_constructor_target(
    env,
    decl.body_start,
    expr_end,
    ctor_name_out,
    arg_starts,
    arg_ends,
    arg_ctor_bindings,
    max_args,
    argc_out,
    depth + 1u
  );
  if (ok) {
    for (uint32_t i = 0u; i < *argc_out; i += 1u) {
      uint32_t resolved_start = 0u;
      uint32_t resolved_end = 0u;
      if (resolve_bound_expr_span(env, arg_starts[i], arg_ends[i], &resolved_start, &resolved_end, 0u)) {
        arg_starts[i] = resolved_start;
        arg_ends[i] = resolved_end;
      }
      if (arg_ctor_bindings &&
          capture_constructor_binding(env, arg_starts[i], arg_ends[i], &arg_ctor_bindings[i], depth + 1u) &&
          arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
        normalize_ctor_binding(
          env,
          arg_ctor_bindings[i],
          arg_starts[i],
          arg_ends[i],
          depth + 1u
        );
        Phase1CtorBinding *binding = &env->ctor_bindings[arg_ctor_bindings[i]];
        arg_starts[i] = binding->expr_start;
        arg_ends[i] = binding->expr_end;
      }
    }
  }
  env->expr_count = saved_expr_count;
  if (!ok) {
    env->ctor_binding_count = saved_ctor_binding_count;
  }
  env->inline_count = saved_inline_count;
  return ok;
}

static int capture_constructor_binding(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *binding_index_out,
  uint32_t depth
) {
  if (!binding_index_out || depth > MAX_PHASE1_EMIT_INLINE_DEPTH) {
    return 0;
  }
  start = skip_expr_ws(env->source, start, end);
  end = trim_expr_end(env->source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_is_wrapped_parens(env->source, start, end)) {
    return capture_constructor_binding(env, start + 1u, end - 1u, binding_index_out, depth + 1u);
  }
  {
    uint32_t next = start;
    NameSpan simple = parse_simple_name_token(env->source, start, end, &next);
    if (simple.ok && next == end) {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (lookup_emit_expr_binding(env, simple, &bound_start, &bound_end, &ctor_binding) &&
          ctor_binding != MISSING_CTOR_BINDING) {
        *binding_index_out = ctor_binding;
        return 1;
      }
    }
  }
  if (env->ctor_binding_count >= MAX_PHASE1_EMIT_CTOR_BINDINGS) {
    return 0;
  }
  NameSpan ctor_name = missing_name_span();
  uint32_t arg_starts[MAX_EVAL_ARGS] = {0};
  uint32_t arg_ends[MAX_EVAL_ARGS] = {0};
  uint32_t arg_ctor_bindings[MAX_EVAL_ARGS];
  init_ctor_binding_array(arg_ctor_bindings, MAX_EVAL_ARGS);
  uint32_t argc = 0u;
  if (!resolve_direct_constructor_target(
        env,
        start,
        end,
        &ctor_name,
        arg_starts,
        arg_ends,
        arg_ctor_bindings,
        MAX_EVAL_ARGS,
        &argc,
        depth + 1u
      )) {
    return 0;
  }
  uint32_t binding_index = env->ctor_binding_count;
  env->ctor_binding_count += 1u;
  Phase1CtorBinding *binding = &env->ctor_bindings[binding_index];
  binding->ctor_name = ctor_name;
  binding->expr_start = start;
  binding->expr_end = end;
  binding->arg_count = argc;
  for (uint32_t i = 0u; i < argc; i += 1u) {
    binding->arg_starts[i] = arg_starts[i];
    binding->arg_ends[i] = arg_ends[i];
    binding->arg_ctor_bindings[i] = arg_ctor_bindings[i];
    if (
      binding->arg_ctor_bindings[i] == MISSING_CTOR_BINDING &&
      capture_constructor_binding(env, arg_starts[i], arg_ends[i], &binding->arg_ctor_bindings[i], depth + 1u)
    ) {
      Phase1CtorBinding *child = &env->ctor_bindings[binding->arg_ctor_bindings[i]];
      binding->arg_starts[i] = child->expr_start;
      binding->arg_ends[i] = child->expr_end;
    }
  }
  *binding_index_out = binding_index;
  return 1;
}

static void normalize_ctor_binding(
  Phase1EmitEnv *env,
  uint32_t binding_index,
  uint32_t expr_start,
  uint32_t expr_end,
  uint32_t depth
) {
  if (!env || binding_index == MISSING_CTOR_BINDING ||
      binding_index >= env->ctor_binding_count ||
      depth > MAX_PHASE1_EMIT_INLINE_DEPTH) {
    return;
  }
  Phase1CtorBinding *binding = &env->ctor_bindings[binding_index];
  binding->expr_start = expr_start;
  binding->expr_end = expr_end;
  for (uint32_t i = 0u; i < binding->arg_count; i += 1u) {
    uint32_t resolved_start = binding->arg_starts[i];
    uint32_t resolved_end = binding->arg_ends[i];
    if (resolve_bound_expr_span(
          env,
          binding->arg_starts[i],
          binding->arg_ends[i],
          &resolved_start,
          &resolved_end,
          0u)) {
      binding->arg_starts[i] = resolved_start;
      binding->arg_ends[i] = resolved_end;
    }
    if (binding->arg_ctor_bindings[i] == MISSING_CTOR_BINDING) {
      capture_constructor_binding(
        env,
        binding->arg_starts[i],
        binding->arg_ends[i],
        &binding->arg_ctor_bindings[i],
        depth + 1u
      );
    }
    if (binding->arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
      normalize_ctor_binding(
        env,
        binding->arg_ctor_bindings[i],
        binding->arg_starts[i],
        binding->arg_ends[i],
        depth + 1u
      );
      Phase1CtorBinding *child = &env->ctor_bindings[binding->arg_ctor_bindings[i]];
      binding->arg_starts[i] = child->expr_start;
      binding->arg_ends[i] = child->expr_end;
    }
  }
}

static int phase1_find_rightmost_binary_operator(
  Segment source,
  uint32_t start,
  uint32_t end,
  const char **ops,
  const uint8_t *opcodes,
  uint32_t op_count,
  uint32_t *split_at,
  uint32_t *op_len_out,
  uint8_t *opcode_out
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t best_at = 0u;
  uint32_t best_len = 0u;
  uint8_t best_opcode = 0u;
  int found = 0;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = start; at < end; at += 1u) {
    uint8_t c = mem[at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == '-' && at + 1u < end && mem[at + 1u] == '-') {
      while (at < end && mem[at] != '\n' &&
             !(at + 1u < end && mem[at] == '\\' &&
               (mem[at + 1u] == 'n' || mem[at + 1u] == 'r'))) {
        at += 1u;
      }
      continue;
    }
    if (c == '(') {
      depth += 1u;
      continue;
    }
    if (c == ')' && depth > 0u) {
      depth -= 1u;
      continue;
    }
    if (depth != 0u) {
      continue;
    }
    for (uint32_t i = 0u; i < op_count; i += 1u) {
      uint32_t op_len = cstr_len(ops[i]);
      if (at + op_len > end) {
        continue;
      }
      int matches = 1;
      for (uint32_t j = 0u; j < op_len; j += 1u) {
        if (mem[at + j] != (uint8_t) ops[i][j]) {
          matches = 0;
          break;
        }
      }
      if (!matches) {
        continue;
      }
      uint32_t lhs_end = trim_expr_end(source, start, at);
      uint32_t rhs_start = skip_expr_ws(source, at + op_len, end);
      if (lhs_end <= start || rhs_start >= end) {
        continue;
      }
      best_at = at;
      best_len = op_len;
      best_opcode = opcodes[i];
      found = 1;
    }
  }
  if (!found) {
    return 0;
  }
  *split_at = best_at;
  *op_len_out = best_len;
  *opcode_out = best_opcode;
  return 1;
}

static int emit_expr_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
);

static int emit_bound_apply_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *extra_arg_starts,
  uint32_t *extra_arg_ends,
  uint32_t extra_argc,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
);

static int emit_lambda_apply_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = 0u;
  uint32_t body_start = 0u;
  if (!parse_lambda_expr(
        env->source,
        start,
        end,
        params,
        MAX_EVAL_ARGS,
        &param_count,
        &body_start)) {
    return 0;
  }
  if (param_count == 0u || argc < param_count ||
      env->expr_count + param_count > MAX_PHASE1_EMIT_EXPR_BINDINGS) {
    return 0;
  }
  uint32_t saved_expr_count = env->expr_count;
  uint32_t saved_ctor_binding_count = env->ctor_binding_count;
  for (uint32_t i = 0u; i < param_count; i += 1u) {
    uint32_t resolved_start = arg_starts[i];
    uint32_t resolved_end = arg_ends[i];
    resolve_bound_expr_span(
      env,
      arg_starts[i],
      arg_ends[i],
      &resolved_start,
      &resolved_end,
      0u
    );
    if (!bind_emit_expr_name(env, params[i], resolved_start, resolved_end)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
  }
  int ok = 0;
  if (argc == param_count) {
    ok = emit_expr_wasm(env, body_start, end, buf, cap, at, depth + 1u);
  } else {
    ok = emit_bound_apply_wasm(
      env,
      body_start,
      end,
      arg_starts + param_count,
      arg_ends + param_count,
      argc - param_count,
      buf,
      cap,
      at,
      depth + 1u
    );
  }
  env->expr_count = saved_expr_count;
  env->ctor_binding_count = saved_ctor_binding_count;
  return ok;
}
static int emit_bound_apply_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *extra_arg_starts,
  uint32_t *extra_arg_ends,
  uint32_t extra_argc,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
);

static int emit_top_level_name_or_atom_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  start = skip_expr_ws(env->source, start, end);
  end = trim_expr_end(env->source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_is_wrapped_parens(env->source, start, end)) {
    return emit_expr_wasm(env, start + 1u, end - 1u, buf, cap, at, depth + 1u);
  }
  {
    int32_t int_value = 0;
    uint32_t next = start;
    if (parse_signed_int_literal(env->source, start, end, &int_value, &next) &&
        next == end) {
      return append_i32_const_instr(buf, cap, at, int_value);
    }
  }
  {
    uint32_t next = start;
    NameSpan name = parse_simple_name_token(env->source, start, end, &next);
    if (!name.ok || next != end) {
      return 0;
    }
    if (namespan_equals_literal(name, "true") || namespan_equals_literal(name, "True")) {
      return append_i32_const_instr(buf, cap, at, 1);
    }
    if (namespan_equals_literal(name, "false") || namespan_equals_literal(name, "False")) {
      return append_i32_const_instr(buf, cap, at, 0);
    }
    {
      uint32_t local_index = 0u;
      if (lookup_emit_local_index(env, name, &local_index)) {
        return append_local_get_instr(buf, cap, at, local_index);
      }
    }
    {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      if (lookup_emit_expr_binding(env, name, &bound_start, &bound_end, NULL)) {
        return emit_expr_wasm(env, bound_start, bound_end, buf, cap, at, depth + 1u);
      }
    }
    {
      int decl_index = find_decl_index_by_name(env->decls, env->decl_count, name);
      if (decl_index >= 0 && decl_param_count(env->source, env->decls[(uint32_t) decl_index]) == 0u) {
        if (env->function_index_by_decl[decl_index] < 0) {
          FnDecl decl = env->decls[(uint32_t) decl_index];
          uint32_t expr_end = decl.body_end > decl.body_start
            ? decl.body_end
            : decl_expression_end(env->source, env->decls, env->decl_count, (uint32_t) decl_index);
          return emit_expr_wasm(env, decl.body_start, expr_end, buf, cap, at, depth + 1u);
        }
        return append_call_instr(
          buf,
          cap,
          at,
          (uint32_t) env->function_index_by_decl[decl_index]
        );
      }
    }
  }
  return 0;
}

static int emit_inline_named_decl_wasm(
  Phase1EmitEnv *env,
  NameSpan head,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  if (env->inline_count >= MAX_PHASE1_EMIT_INLINE_DEPTH || emit_inline_stack_has(env, head)) {
    return 0;
  }
  int decl_index = find_decl_index_by_name(env->decls, env->decl_count, head);
  if (decl_index < 0) {
    return 0;
  }
  FnDecl decl = env->decls[(uint32_t) decl_index];
  NameSpan params[MAX_EVAL_ARGS];
  uint32_t param_count = collect_decl_params(env->source, decl, params, MAX_EVAL_ARGS);
  if (param_count != argc || env->expr_count + argc > MAX_PHASE1_EMIT_EXPR_BINDINGS) {
    return 0;
  }
  uint32_t saved_expr_count = env->expr_count;
  uint32_t saved_ctor_binding_count = env->ctor_binding_count;
  uint32_t saved_inline_count = env->inline_count;
  for (uint32_t i = 0u; i < argc; i += 1u) {
    uint32_t original_next = arg_starts[i];
    uint32_t original_ctor_binding = MISSING_CTOR_BINDING;
    uint32_t original_bound_start = 0u;
    uint32_t original_bound_end = 0u;
    NameSpan original_name = parse_simple_name_token(env->source, arg_starts[i], arg_ends[i], &original_next);
    if (original_name.ok && original_next == arg_ends[i]) {
      lookup_emit_expr_binding(
        env,
        original_name,
        &original_bound_start,
        &original_bound_end,
        &original_ctor_binding
      );
    }
    uint32_t resolved_start = arg_starts[i];
    uint32_t resolved_end = arg_ends[i];
    resolve_bound_expr_span(
      env,
      arg_starts[i],
      arg_ends[i],
      &resolved_start,
      &resolved_end,
      0u
    );
    if (original_ctor_binding != MISSING_CTOR_BINDING) {
      if (!bind_emit_expr_name_with_ctor(
            env,
            params[i],
            original_bound_start,
            original_bound_end,
            original_ctor_binding)) {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        env->inline_count = saved_inline_count;
        return 0;
      }
      continue;
    }
    {
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (capture_constructor_binding(env, arg_starts[i], arg_ends[i], &ctor_binding, 0u)) {
        if (!bind_emit_expr_name_with_ctor(
              env,
              params[i],
              resolved_start,
              resolved_end,
              ctor_binding)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          env->inline_count = saved_inline_count;
          return 0;
        }
        continue;
      }
    }
    if (!bind_emit_expr_name(env, params[i], resolved_start, resolved_end)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      env->inline_count = saved_inline_count;
      return 0;
    }
  }
  env->inline_stack[env->inline_count] = head;
  env->inline_count += 1u;
  uint32_t expr_end = decl_expression_end(env->source, env->decls, env->decl_count, (uint32_t) decl_index);
  int ok = emit_expr_wasm(env, decl.body_start, expr_end, buf, cap, at, depth + 1u);
  env->expr_count = saved_expr_count;
  env->ctor_binding_count = saved_ctor_binding_count;
  env->inline_count = saved_inline_count;
  return ok;
}

static int emit_named_apply_wasm(
  Phase1EmitEnv *env,
  NameSpan head,
  uint32_t *arg_starts,
  uint32_t *arg_ends,
  uint32_t argc,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  if (emit_inline_named_decl_wasm(env, head, arg_starts, arg_ends, argc, buf, cap, at, depth + 1u)) {
    return 1;
  }
  {
    int decl_index = find_decl_index_by_name(env->decls, env->decl_count, head);
    if (decl_index >= 0) {
      FnDecl decl = env->decls[(uint32_t) decl_index];
      uint32_t param_count = decl_param_count(env->source, decl);
      if (param_count > 0u && argc > param_count &&
          env->inline_count < MAX_PHASE1_EMIT_INLINE_DEPTH &&
          !emit_inline_stack_has(env, head) &&
          env->expr_count + param_count <= MAX_PHASE1_EMIT_EXPR_BINDINGS) {
        NameSpan params[MAX_EVAL_ARGS] = {0};
        uint32_t collected = collect_decl_params(env->source, decl, params, MAX_EVAL_ARGS);
        if (collected == param_count) {
          uint32_t saved_expr_count = env->expr_count;
          uint32_t saved_ctor_binding_count = env->ctor_binding_count;
          uint32_t saved_inline_count = env->inline_count;
          for (uint32_t i = 0u; i < param_count; i += 1u) {
            uint32_t resolved_start = arg_starts[i];
            uint32_t resolved_end = arg_ends[i];
            resolve_bound_expr_span(
              env,
              arg_starts[i],
              arg_ends[i],
              &resolved_start,
              &resolved_end,
              0u
            );
            if (!bind_emit_expr_name(env, params[i], resolved_start, resolved_end)) {
              env->expr_count = saved_expr_count;
              env->ctor_binding_count = saved_ctor_binding_count;
              env->inline_count = saved_inline_count;
              return 0;
            }
          }
          env->inline_stack[env->inline_count] = head;
          env->inline_count += 1u;
          uint32_t expr_end = decl.body_end > decl.body_start
            ? decl.body_end
            : decl_expression_end(
              env->source,
              env->decls,
              env->decl_count,
              (uint32_t) decl_index
          );
          int ok = emit_bound_apply_wasm(
            env,
            decl.body_start,
            expr_end,
            arg_starts + param_count,
            arg_ends + param_count,
            argc - param_count,
            buf,
            cap,
            at,
            depth + 1u
          );
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          env->inline_count = saved_inline_count;
          if (ok) {
            return 1;
          }
        }
      }
    }
  }
  if (namespan_equals_literal(head, "add") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6au);
  }
  if (namespan_equals_literal(head, "sub") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6bu);
  }
  if (namespan_equals_literal(head, "mul") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6cu);
  }
  if (namespan_equals_literal(head, "div") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6du);
  }
  if (namespan_equals_literal(head, "mod") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6fu);
  }
  if (namespan_equals_literal(head, "eq") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x46u);
  }
  if (namespan_equals_literal(head, "ne") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x47u);
  }
  if (namespan_equals_literal(head, "lt") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x48u);
  }
  if (namespan_equals_literal(head, "gt") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x4au);
  }
  if (namespan_equals_literal(head, "le") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x4cu);
  }
  if (namespan_equals_literal(head, "ge") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x4eu);
  }
  if (namespan_equals_literal(head, "and") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x71u);
  }
  if (namespan_equals_literal(head, "or") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x72u);
  }
  if (namespan_equals_literal(head, "not") && argc == 1u) {
    if (!emit_expr_wasm(env, arg_starts[0], arg_ends[0], buf, cap, at, depth + 1u)) {
      return 0;
    }
    return append_buf_u8(buf, cap, at, 0x45u);
  }
  {
    int decl_index = find_decl_index_by_name(env->decls, env->decl_count, head);
    if (decl_index >= 0 &&
        env->function_index_by_decl[decl_index] >= 0 &&
        decl_param_count(env->source, env->decls[(uint32_t) decl_index]) == argc) {
      for (uint32_t i = 0u; i < argc; i += 1u) {
        if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
          return 0;
        }
      }
      return append_call_instr(buf, cap, at, (uint32_t) env->function_index_by_decl[decl_index]);
    }
  }
  return 0;
}

static int emit_bound_apply_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint32_t *extra_arg_starts,
  uint32_t *extra_arg_ends,
  uint32_t extra_argc,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  if (depth > 16u) {
    return 0;
  }
  if (!resolve_bound_expr_span(env, start, end, &start, &end, 0u)) {
    return 0;
  }
  if (emit_lambda_apply_wasm(
        env,
        start,
        end,
        extra_arg_starts,
        extra_arg_ends,
        extra_argc,
        buf,
        cap,
        at,
        depth + 1u)) {
    return 1;
  }
  if (span_is_wrapped_parens(env->source, start, end)) {
    return emit_bound_apply_wasm(env, start + 1u, end - 1u, extra_arg_starts, extra_arg_ends, extra_argc, buf, cap, at, depth + 1u);
  }
  {
    uint32_t next = start;
    NameSpan simple = parse_simple_name_token(env->source, start, end, &next);
    if (simple.ok && next == end) {
      uint32_t bound_start = 0u;
      uint32_t bound_end = 0u;
      if (lookup_emit_expr_binding(env, simple, &bound_start, &bound_end, NULL)) {
        return emit_bound_apply_wasm(env, bound_start, bound_end, extra_arg_starts, extra_arg_ends, extra_argc, buf, cap, at, depth + 1u);
      }
      {
        int decl_index = find_decl_index_by_name(env->decls, env->decl_count, simple);
        if (decl_index >= 0 && decl_param_count(env->source, env->decls[(uint32_t) decl_index]) == 0u) {
          FnDecl decl = env->decls[(uint32_t) decl_index];
          uint32_t expr_end = decl.body_end > decl.body_start
            ? decl.body_end
            : decl_expression_end(env->source, env->decls, env->decl_count, (uint32_t) decl_index);
          return emit_bound_apply_wasm(env, decl.body_start, expr_end, extra_arg_starts, extra_arg_ends, extra_argc, buf, cap, at, depth + 1u);
        }
      }
    }
  }
  {
    NameSpan head = missing_name_span();
    uint32_t arg_starts[MAX_EVAL_ARGS] = {0};
    uint32_t arg_ends[MAX_EVAL_ARGS] = {0};
    uint32_t argc = 0u;
    if (!parse_apply_span(env->source, start, end, &head, arg_starts, arg_ends, MAX_EVAL_ARGS, &argc)) {
      return 0;
    }
    if (argc + extra_argc > MAX_EVAL_ARGS) {
      return 0;
    }
    for (uint32_t i = 0u; i < extra_argc; i += 1u) {
      arg_starts[argc + i] = extra_arg_starts[i];
      arg_ends[argc + i] = extra_arg_ends[i];
    }
    return emit_named_apply_wasm(env, head, arg_starts, arg_ends, argc + extra_argc, buf, cap, at, depth + 1u);
  }
}

static int emit_prefix_apply_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  uint32_t head_start = skip_expr_ws(env->source, start, end);
  uint32_t head_end = parse_expr_atom_end(env->source, head_start, end);
  if (head_end <= head_start) {
    return 0;
  }
  uint32_t cursor = skip_expr_ws(env->source, head_end, end);
  if (cursor >= end) {
    return emit_top_level_name_or_atom_wasm(env, head_start, head_end, buf, cap, at, depth + 1u);
  }
  uint32_t arg_starts[MAX_EVAL_ARGS] = {0};
  uint32_t arg_ends[MAX_EVAL_ARGS] = {0};
  uint32_t argc = 0u;
  while (cursor < end) {
    if (argc >= MAX_EVAL_ARGS) {
      return 0;
    }
    uint32_t atom_end = parse_expr_atom_end(env->source, cursor, end);
    if (atom_end <= cursor) {
      return 0;
    }
    arg_starts[argc] = cursor;
    arg_ends[argc] = atom_end;
    argc += 1u;
    cursor = skip_expr_ws(env->source, atom_end, end);
  }
  if (span_is_wrapped_parens(env->source, head_start, head_end)) {
    return emit_bound_apply_wasm(
      env,
      head_start + 1u,
      head_end - 1u,
      arg_starts,
      arg_ends,
      argc,
      buf,
      cap,
      at,
      depth + 1u
    );
  }
  uint32_t next = head_start;
  NameSpan head = parse_simple_name_token(env->source, head_start, head_end, &next);
  if (!head.ok || next != head_end) {
    return 0;
  }
  if (namespan_equals_literal(head, "id") && argc == 1u) {
    return emit_expr_wasm(env, arg_starts[0], arg_ends[0], buf, cap, at, depth + 1u);
  }
  {
    NameSpan get_tag = missing_name_span();
    uint32_t get_index = 0u;
    if (argc == 1u && parse_get_helper_name(env->source, head, &get_tag, &get_index)) {
      uint32_t mk_start = 0u;
      uint32_t mk_end = 0u;
      if (resolve_bound_expr_span(env, arg_starts[0], arg_ends[0], &mk_start, &mk_end, 0u)) {
        NameSpan mk_head = missing_name_span();
        uint32_t mk_arg_starts[MAX_EVAL_ARGS] = {0};
        uint32_t mk_arg_ends[MAX_EVAL_ARGS] = {0};
        uint32_t mk_argc = 0u;
        if (parse_apply_span(
              env->source,
              mk_start,
              mk_end,
              &mk_head,
              mk_arg_starts,
              mk_arg_ends,
              MAX_EVAL_ARGS,
              &mk_argc)) {
          NameSpan mk_tag = missing_name_span();
          uint32_t mk_arity = 0u;
          if (parse_mk_helper_name(env->source, mk_head, &mk_tag, &mk_arity)) {
            if (!helper_tags_equal(env->source, get_tag, env->source, mk_tag) ||
                get_index >= mk_argc || mk_argc != mk_arity) {
              return append_buf_u8(buf, cap, at, 0x00u);
            }
            return emit_expr_wasm(
              env,
              mk_arg_starts[get_index],
              mk_arg_ends[get_index],
              buf,
              cap,
              at,
              depth + 1u
            );
          }
        }
      }
    }
  }
  {
    NameSpan is_tag = missing_name_span();
    if (argc == 1u && parse_is_helper_name(env->source, head, &is_tag)) {
      uint32_t mk_start = 0u;
      uint32_t mk_end = 0u;
      if (resolve_bound_expr_span(env, arg_starts[0], arg_ends[0], &mk_start, &mk_end, 0u)) {
        NameSpan mk_head = missing_name_span();
        uint32_t mk_arg_starts[MAX_EVAL_ARGS] = {0};
        uint32_t mk_arg_ends[MAX_EVAL_ARGS] = {0};
        uint32_t mk_argc = 0u;
        if (parse_apply_span(
              env->source,
              mk_start,
              mk_end,
              &mk_head,
              mk_arg_starts,
              mk_arg_ends,
              MAX_EVAL_ARGS,
              &mk_argc)) {
          NameSpan mk_tag = missing_name_span();
          uint32_t mk_arity = 0u;
          if (parse_mk_helper_name(env->source, mk_head, &mk_tag, &mk_arity) &&
              mk_argc == mk_arity) {
            return append_i32_const_instr(
              buf,
              cap,
              at,
              helper_tags_equal(env->source, is_tag, env->source, mk_tag) ? 1 : 0
            );
          }
        }
      }
    }
  }
  if (emit_named_apply_wasm(env, head, arg_starts, arg_ends, argc, buf, cap, at, depth + 1u)) {
    return 1;
  }
  if (namespan_equals_literal(head, "add") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6au);
  }
  if (namespan_equals_literal(head, "sub") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6bu);
  }
  if (namespan_equals_literal(head, "mul") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6cu);
  }
  if (namespan_equals_literal(head, "div") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6du);
  }
  if (namespan_equals_literal(head, "mod") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x6fu);
  }
  if (namespan_equals_literal(head, "eq") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x46u);
  }
  if (namespan_equals_literal(head, "ne") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x47u);
  }
  if (namespan_equals_literal(head, "lt") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x48u);
  }
  if (namespan_equals_literal(head, "gt") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x4au);
  }
  if (namespan_equals_literal(head, "le") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x4cu);
  }
  if (namespan_equals_literal(head, "ge") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x4eu);
  }
  if (namespan_equals_literal(head, "and") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x71u);
  }
  if (namespan_equals_literal(head, "or") && argc == 2u) {
    for (uint32_t i = 0u; i < argc; i += 1u) {
      if (!emit_expr_wasm(env, arg_starts[i], arg_ends[i], buf, cap, at, depth + 1u)) {
        return 0;
      }
    }
    return append_buf_u8(buf, cap, at, 0x72u);
  }
  if (namespan_equals_literal(head, "not") && argc == 1u) {
    if (!emit_expr_wasm(env, arg_starts[0], arg_ends[0], buf, cap, at, depth + 1u)) {
      return 0;
    }
    return append_buf_u8(buf, cap, at, 0x45u);
  }
  {
    uint32_t bound_start = 0u;
    uint32_t bound_end = 0u;
    if (lookup_emit_expr_binding(env, head, &bound_start, &bound_end, NULL)) {
      return emit_bound_apply_wasm(env, bound_start, bound_end, arg_starts, arg_ends, argc, buf, cap, at, depth + 1u);
    }
  }
  {
    int decl_index = find_decl_index_by_name(env->decls, env->decl_count, head);
    if (decl_index >= 0 && decl_param_count(env->source, env->decls[(uint32_t) decl_index]) == 0u) {
      FnDecl decl = env->decls[(uint32_t) decl_index];
      uint32_t expr_end = decl.body_end > decl.body_start
        ? decl.body_end
        : decl_expression_end(env->source, env->decls, env->decl_count, (uint32_t) decl_index);
      return emit_bound_apply_wasm(env, decl.body_start, expr_end, arg_starts, arg_ends, argc, buf, cap, at, depth + 1u);
    }
  }
  return 0;
}

static int emit_if_expr_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  if (!span_matches_keyword(env->source, start, end, "if")) {
    return 0;
  }
  uint8_t *mem = (uint8_t *) (uintptr_t) env->source.ptr;
  uint32_t cursor = skip_expr_ws(env->source, start + 2u, end);
  uint32_t then_at = cursor;
  uint32_t nesting = 0u;
  int in_string = 0;
  int escaped = 0;
  while (then_at < end) {
    uint8_t c = mem[then_at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      then_at += 1u;
      continue;
    }
    if (c == '"') {
      in_string = 1;
      then_at += 1u;
      continue;
    }
    if (c == '(') {
      nesting += 1u;
    } else if (c == ')' && nesting > 0u) {
      nesting -= 1u;
    } else if (nesting == 0u && match_keyword_at(env->source, then_at, start, end, "then")) {
      break;
    }
    then_at += 1u;
  }
  if (then_at >= end) {
    return 0;
  }
  uint32_t else_at = skip_expr_ws(env->source, then_at + 4u, end);
  uint32_t branch_split = else_at;
  nesting = 0u;
  in_string = 0;
  escaped = 0;
  while (branch_split < end) {
    uint8_t c = mem[branch_split];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      branch_split += 1u;
      continue;
    }
    if (c == '"') {
      in_string = 1;
      branch_split += 1u;
      continue;
    }
    if (c == '(') {
      nesting += 1u;
    } else if (c == ')' && nesting > 0u) {
      nesting -= 1u;
    } else if (nesting == 0u && match_keyword_at(env->source, branch_split, start, end, "else")) {
      break;
    }
    branch_split += 1u;
  }
  if (branch_split >= end) {
    return 0;
  }
  if (!emit_expr_wasm(env, cursor, then_at, buf, cap, at, depth + 1u)) {
    return 0;
  }
  if (!append_buf_u8(buf, cap, at, 0x04u) ||
      !append_buf_u8(buf, cap, at, 0x7fu)) {
    return 0;
  }
  if (!emit_expr_wasm(env, else_at, branch_split, buf, cap, at, depth + 1u)) {
    return 0;
  }
  if (!append_buf_u8(buf, cap, at, 0x05u)) {
    return 0;
  }
  if (!emit_expr_wasm(env, branch_split + 4u, end, buf, cap, at, depth + 1u)) {
    return 0;
  }
  return append_buf_u8(buf, cap, at, 0x0bu);
}

static int find_let_binding_split(
  Segment source,
  uint32_t start,
  uint32_t end,
  uint32_t line_end,
  uint32_t *value_end,
  uint32_t *next_cursor,
  int *found_in
) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = start; at < line_end; at += 1u) {
    uint8_t c = mem[at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == '(') {
      depth += 1u;
      continue;
    }
    if (c == ')' && depth > 0u) {
      depth -= 1u;
      continue;
    }
    if (depth != 0u) {
      continue;
    }
    if (c == ';') {
      *value_end = at;
      *next_cursor = at + 1u;
      *found_in = 0;
      return 1;
    }
    if (match_keyword_at(source, at, start, end, "in")) {
      *value_end = at;
      *next_cursor = at + 2u;
      *found_in = 1;
      return 1;
    }
  }
  *value_end = line_end;
  *next_cursor = source_next_line_start(source, line_end);
  *found_in = 0;
  return 1;
}

static int append_emit_eq_const_condition(
  Phase1EmitEnv *env,
  uint32_t target_start,
  uint32_t target_end,
  int32_t expected,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t *cond_count,
  uint32_t depth
) {
  if (!emit_expr_wasm(env, target_start, target_end, buf, cap, at, depth + 1u) ||
      !append_i32_const_instr(buf, cap, at, expected) ||
      !append_buf_u8(buf, cap, at, 0x46u)) {
    return 0;
  }
  if (*cond_count > 0u && !append_buf_u8(buf, cap, at, 0x71u)) {
    return 0;
  }
  *cond_count += 1u;
  return 1;
}

static int bind_emit_expr_name_with_ctor(
  Phase1EmitEnv *env,
  NameSpan name,
  uint32_t expr_start,
  uint32_t expr_end,
  uint32_t ctor_binding
) {
  if (env->expr_count >= MAX_PHASE1_EMIT_EXPR_BINDINGS) {
    return 0;
  }
  env->expr_names[env->expr_count] = name;
  env->expr_starts[env->expr_count] = expr_start;
  env->expr_ends[env->expr_count] = expr_end;
  env->expr_ctor_bindings[env->expr_count] = ctor_binding;
  env->expr_count += 1u;
  return 1;
}

static int bind_emit_expr_name(
  Phase1EmitEnv *env,
  NameSpan name,
  uint32_t expr_start,
  uint32_t expr_end
) {
  if (span_is_exact_simple_name(env->source, expr_start, expr_end, name)) {
    return 1;
  }
  uint32_t ctor_binding = MISSING_CTOR_BINDING;
  if (capture_constructor_binding(env, expr_start, expr_end, &ctor_binding, 0u)) {
    Phase1CtorBinding *binding = &env->ctor_bindings[ctor_binding];
    return bind_emit_expr_name_with_ctor(
      env,
      name,
      binding->expr_start,
      binding->expr_end,
      ctor_binding
    );
  }
  return bind_emit_expr_name_with_ctor(
    env,
    name,
    expr_start,
    expr_end,
    MISSING_CTOR_BINDING
  );
}

static int append_emit_guard_condition(
  Phase1EmitEnv *env,
  uint32_t guard_start,
  uint32_t guard_end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t *cond_count,
  uint32_t depth
) {
  guard_start = skip_expr_ws(env->source, guard_start, guard_end);
  guard_end = trim_expr_end(env->source, guard_start, guard_end);
  if (guard_start >= guard_end) {
    return 1;
  }
  {
    uint32_t next = guard_start;
    NameSpan token = parse_simple_name_token(env->source, guard_start, guard_end, &next);
    if (token.ok && next == guard_end) {
      if (namespan_equals_literal(token, "otherwise") ||
          namespan_equals_literal(token, "true") ||
          namespan_equals_literal(token, "True")) {
        return 1;
      }
      if (namespan_equals_literal(token, "false") ||
          namespan_equals_literal(token, "False")) {
        if (!append_i32_const_instr(buf, cap, at, 0)) {
          return 0;
        }
        if (*cond_count > 0u && !append_buf_u8(buf, cap, at, 0x71u)) {
          return 0;
        }
        *cond_count += 1u;
        return 1;
      }
    }
  }
  if (!emit_expr_wasm(env, guard_start, guard_end, buf, cap, at, depth + 1u)) {
    return 0;
  }
  if (*cond_count > 0u && !append_buf_u8(buf, cap, at, 0x71u)) {
    return 0;
  }
  *cond_count += 1u;
  return 1;
}

static int emit_simple_pattern_condition(
  Phase1EmitEnv *env,
  uint32_t pattern_start,
  uint32_t pattern_end,
  uint32_t target_start,
  uint32_t target_end,
  uint32_t target_ctor_binding,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t *cond_count,
  uint32_t depth
) {
  pattern_start = skip_expr_ws(env->source, pattern_start, pattern_end);
  pattern_end = trim_expr_end(env->source, pattern_start, pattern_end);
  if (pattern_start >= pattern_end) {
    return 0;
  }
  {
    uint8_t *mem = (uint8_t *) (uintptr_t) env->source.ptr;
    if (pattern_end == pattern_start + 1u && mem[pattern_start] == '_') {
      return 1;
    }
  }
  {
    int32_t int_value = 0;
    uint32_t next = pattern_start;
    if (parse_signed_int_literal(env->source, pattern_start, pattern_end, &int_value, &next) &&
        next == pattern_end) {
      return append_emit_eq_const_condition(
        env,
        target_start,
        target_end,
        int_value,
        buf,
        cap,
        at,
        cond_count,
        depth + 1u
      );
    }
  }
  {
    uint32_t next = pattern_start;
    NameSpan token = parse_simple_name_token(env->source, pattern_start, pattern_end, &next);
    if (!token.ok || next != pattern_end) {
      return 0;
    }
    if (namespan_equals_literal(token, "true") || namespan_equals_literal(token, "True")) {
      return append_emit_eq_const_condition(
        env,
        target_start,
        target_end,
        1,
        buf,
        cap,
        at,
        cond_count,
        depth + 1u
      );
    }
    if (namespan_equals_literal(token, "false") || namespan_equals_literal(token, "False")) {
      return append_emit_eq_const_condition(
        env,
        target_start,
        target_end,
        0,
        buf,
        cap,
        at,
        cond_count,
        depth + 1u
      );
    }
    {
      uint32_t ctor_binding = target_ctor_binding;
      if (ctor_binding == MISSING_CTOR_BINDING &&
          capture_constructor_binding(env, target_start, target_end, &ctor_binding, depth + 1u) &&
          ctor_binding != MISSING_CTOR_BINDING) {
        return bind_emit_expr_name_with_ctor(
          env,
          token,
          target_start,
          target_end,
          ctor_binding
        );
      }
      if (ctor_binding != MISSING_CTOR_BINDING) {
        return bind_emit_expr_name_with_ctor(
          env,
          token,
          target_start,
          target_end,
          ctor_binding
        );
      }
    }
    return bind_emit_expr_name(env, token, target_start, target_end);
  }
}

static int emit_clause_decl_lines_wasm(
  Phase1EmitEnv *env,
  uint32_t *param_starts,
  uint32_t *param_ends,
  uint32_t param_count,
  uint32_t line_start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  while (line_start < end) {
    line_start = skip_expr_ws(env->source, line_start, end);
    if (line_start >= end) {
      return 0;
    }
    uint32_t line_end = source_line_end(env->source, line_start);
    if (line_end > end) {
      line_end = end;
    }
    uint32_t next_line = source_next_line_start(env->source, line_end);
    uint32_t cursor = source_skip_line_ws(env->source, line_start, line_end);
    uint8_t *mem = (uint8_t *) (uintptr_t) env->source.ptr;
    if (cursor >= line_end || (mem[cursor] == '-' && cursor + 1u < line_end && mem[cursor + 1u] == '-')) {
      line_start = next_line;
      continue;
    }
    uint32_t eq_at = find_top_level_assignment_eq(env->source, cursor, line_end);
    if (eq_at >= line_end) {
      return 0;
    }
    uint32_t saved_expr_count = env->expr_count;
    uint32_t saved_ctor_binding_count = env->ctor_binding_count;
    uint8_t cond_buf[512] = {0};
    uint32_t cond_at = 0u;
    uint32_t cond_count = 0u;
    uint32_t pat_cursor = cursor;
    pat_cursor = skip_expr_ws(env->source, pat_cursor, eq_at);
    if (!(pat_cursor < eq_at && mem[pat_cursor] == '|')) {
      for (uint32_t arg_index = 0u; arg_index < param_count; arg_index += 1u) {
        pat_cursor = skip_expr_ws(env->source, pat_cursor, eq_at);
        if (pat_cursor >= eq_at || mem[pat_cursor] == '|') {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        uint32_t pat_end = parse_expr_atom_end(env->source, pat_cursor, eq_at);
        if (pat_end <= pat_cursor) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        if (!emit_simple_pattern_condition(
              env,
              pat_cursor,
              pat_end,
              param_starts[arg_index],
              param_ends[arg_index],
              MISSING_CTOR_BINDING,
              cond_buf,
              sizeof(cond_buf),
              &cond_at,
              &cond_count,
              depth + 1u)) {
          env->expr_count = saved_expr_count;
          return 0;
        }
        pat_cursor = pat_end;
      }
    }
    pat_cursor = skip_expr_ws(env->source, pat_cursor, eq_at);
    if (pat_cursor < eq_at) {
      if (mem[pat_cursor] != '|') {
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return 0;
      }
      if (!append_emit_guard_condition(
            env,
            pat_cursor + 1u,
            eq_at,
            cond_buf,
            sizeof(cond_buf),
            &cond_at,
            &cond_count,
            depth + 1u)) {
        env->expr_count = saved_expr_count;
        return 0;
      }
    }
    if (cond_count == 0u) {
      int ok = emit_expr_wasm(env, eq_at + 1u, line_end, buf, cap, at, depth + 1u);
      env->expr_count = saved_expr_count;
      return ok;
    }
    if (!raw_emit_append_bytes(buf, at, cap, cond_buf, cond_at) ||
        !append_buf_u8(buf, cap, at, 0x04u) ||
        !append_buf_u8(buf, cap, at, 0x7fu) ||
        !emit_expr_wasm(env, eq_at + 1u, line_end, buf, cap, at, depth + 1u) ||
        !append_buf_u8(buf, cap, at, 0x05u)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
    env->expr_count = saved_expr_count;
    if (!emit_clause_decl_lines_wasm(
          env,
          param_starts,
          param_ends,
          param_count,
          next_line,
          end,
          buf,
          cap,
          at,
          depth + 1u) ||
        !append_buf_u8(buf, cap, at, 0x0bu)) {
      return 0;
    }
    return 1;
  }
  return 0;
}

static int emit_clause_decl_wasm(
  Phase1EmitEnv *env,
  FnDecl decl,
  uint32_t function_end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  NameSpan params[MAX_EVAL_ARGS] = {0};
  uint32_t param_count = collect_decl_params(env->source, decl, params, MAX_EVAL_ARGS);
  if (param_count != env->param_count) {
    return 0;
  }
  uint32_t param_starts[MAX_EVAL_ARGS];
  uint32_t param_ends[MAX_EVAL_ARGS];
  for (uint32_t i = 0u; i < param_count; i += 1u) {
    param_starts[i] = params[i].ptr - env->source.ptr;
    param_ends[i] = param_starts[i] + params[i].len;
  }
  return emit_clause_decl_lines_wasm(
    env,
    param_starts,
    param_ends,
    param_count,
    decl.body_start,
    function_end,
    buf,
    cap,
    at,
    depth + 1u
  );
}

static uint32_t find_case_of_at(Segment source, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = start; at < end; at += 1u) {
    uint8_t c = mem[at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == '(') {
      depth += 1u;
      continue;
    }
    if (c == ')' && depth > 0u) {
      depth -= 1u;
      continue;
    }
    if (depth == 0u && match_keyword_at(source, at, start, end, "of")) {
      return at;
    }
  }
  return end;
}

static uint32_t find_case_arm_arrow(Segment source, uint32_t start, uint32_t end) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t depth = 0u;
  int in_string = 0;
  int escaped = 0;
  for (uint32_t at = start; at + 1u < end; at += 1u) {
    uint8_t c = mem[at];
    if (in_string) {
      if (escaped) {
        escaped = 0;
      } else if (c == '\\') {
        escaped = 1;
      } else if (c == '"') {
        in_string = 0;
      }
      continue;
    }
    if (c == '"') {
      in_string = 1;
      continue;
    }
    if (c == '(') {
      depth += 1u;
      continue;
    }
    if (c == ')' && depth > 0u) {
      depth -= 1u;
      continue;
    }
    if (depth == 0u && c == '-' && mem[at + 1u] == '>') {
      return at;
    }
  }
  return end;
}

static int emit_case_arms_wasm(
  Phase1EmitEnv *env,
  uint32_t *target_starts,
  uint32_t *target_ends,
  uint32_t target_count,
  uint32_t arms_start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  uint32_t line_start = arms_start;
  while (line_start < end) {
    line_start = skip_expr_ws(env->source, line_start, end);
    if (line_start >= end) {
      return 0;
    }
    uint32_t line_end = source_line_end(env->source, line_start);
    if (line_end > end) {
      line_end = end;
    }
    uint32_t next_line = source_next_line_start(env->source, line_end);
    uint32_t cursor = line_start;
    uint8_t *mem = (uint8_t *) (uintptr_t) env->source.ptr;
    if (cursor >= line_end || (mem[cursor] == '-' && cursor + 1u < line_end && mem[cursor + 1u] == '-')) {
      line_start = next_line;
      continue;
    }
    uint32_t arrow_at = find_case_arm_arrow(env->source, cursor, line_end);
    if (arrow_at >= line_end) {
      return 0;
    }
    uint32_t saved_expr_count = env->expr_count;
    uint32_t saved_ctor_binding_count = env->ctor_binding_count;
    uint8_t cond_buf[512] = {0};
    uint32_t cond_at = 0u;
    uint32_t cond_count = 0u;
    uint32_t pattern_cursor = cursor;
    if (target_count == 1u) {
      uint32_t first_end = parse_expr_atom_end(env->source, pattern_cursor, arrow_at);
      if (first_end <= pattern_cursor) {
        env->expr_count = saved_expr_count;
        return 0;
      }
      uint32_t next = pattern_cursor;
      NameSpan first = parse_simple_name_token(env->source, pattern_cursor, first_end, &next);
      uint32_t after_first = skip_expr_ws(env->source, first_end, arrow_at);
      if (first.ok && next == first_end && namespan_starts_with_upper(env->source, first)) {
        uint32_t ctor_arg_starts[MAX_EVAL_ARGS] = {0};
        uint32_t ctor_arg_ends[MAX_EVAL_ARGS] = {0};
        uint32_t ctor_arg_ctor_bindings[MAX_EVAL_ARGS];
        init_ctor_binding_array(ctor_arg_ctor_bindings, MAX_EVAL_ARGS);
        uint32_t ctor_arg_count = 0u;
        NameSpan ctor_name = missing_name_span();
        if (!resolve_direct_constructor_target(
              env,
              target_starts[0],
              target_ends[0],
              &ctor_name,
              ctor_arg_starts,
              ctor_arg_ends,
              ctor_arg_ctor_bindings,
              MAX_EVAL_ARGS,
              &ctor_arg_count,
              0u)) {
          env->expr_count = saved_expr_count;
          return 0;
        }
        if (!names_equal(first, ctor_name)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          line_start = next_line;
          continue;
        }
        uint32_t arg_index = 0u;
        pattern_cursor = after_first;
        while (pattern_cursor < arrow_at) {
          if (arg_index >= ctor_arg_count) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          uint32_t pat_end = parse_expr_atom_end(env->source, pattern_cursor, arrow_at);
          if (pat_end <= pattern_cursor) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          if (!emit_simple_pattern_condition(
                env,
                pattern_cursor,
                pat_end,
                ctor_arg_starts[arg_index],
                ctor_arg_ends[arg_index],
                ctor_arg_ctor_bindings[arg_index],
                cond_buf,
                sizeof(cond_buf),
                &cond_at,
                &cond_count,
                depth + 1u)) {
            env->expr_count = saved_expr_count;
            env->ctor_binding_count = saved_ctor_binding_count;
            return 0;
          }
          arg_index += 1u;
          pattern_cursor = skip_expr_ws(env->source, pat_end, arrow_at);
        }
        if (arg_index != ctor_arg_count) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
      } else {
        if (!emit_simple_pattern_condition(
              env,
              pattern_cursor,
              arrow_at,
              target_starts[0],
              target_ends[0],
              MISSING_CTOR_BINDING,
              cond_buf,
              sizeof(cond_buf),
              &cond_at,
              &cond_count,
              depth + 1u)) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
      }
    } else {
      for (uint32_t i = 0u; i < target_count; i += 1u) {
        pattern_cursor = skip_expr_ws(env->source, pattern_cursor, arrow_at);
        if (pattern_cursor >= arrow_at) {
          env->expr_count = saved_expr_count;
          env->ctor_binding_count = saved_ctor_binding_count;
          return 0;
        }
        uint32_t pat_end = parse_expr_atom_end(env->source, pattern_cursor, arrow_at);
        if (pat_end <= pattern_cursor) {
          env->expr_count = saved_expr_count;
          return 0;
        }
        if (!emit_simple_pattern_condition(
              env,
              pattern_cursor,
              pat_end,
              target_starts[i],
              target_ends[i],
              MISSING_CTOR_BINDING,
              cond_buf,
              sizeof(cond_buf),
              &cond_at,
              &cond_count,
              depth + 1u)) {
          env->expr_count = saved_expr_count;
          return 0;
        }
        pattern_cursor = pat_end;
      }
      pattern_cursor = skip_expr_ws(env->source, pattern_cursor, arrow_at);
      if (pattern_cursor != arrow_at) {
        env->expr_count = saved_expr_count;
        return 0;
      }
    }
    if (cond_count == 0u) {
      int ok = emit_expr_wasm(env, arrow_at + 2u, line_end, buf, cap, at, depth + 1u);
        env->expr_count = saved_expr_count;
        env->ctor_binding_count = saved_ctor_binding_count;
        return ok;
    }
    if (!raw_emit_append_bytes(buf, at, cap, cond_buf, cond_at) ||
        !append_buf_u8(buf, cap, at, 0x04u) ||
        !append_buf_u8(buf, cap, at, 0x7fu) ||
        !emit_expr_wasm(env, arrow_at + 2u, line_end, buf, cap, at, depth + 1u) ||
        !append_buf_u8(buf, cap, at, 0x05u)) {
      env->expr_count = saved_expr_count;
      env->ctor_binding_count = saved_ctor_binding_count;
      return 0;
    }
    env->expr_count = saved_expr_count;
    if (!emit_case_arms_wasm(
          env,
          target_starts,
          target_ends,
          target_count,
          next_line,
          end,
          buf,
          cap,
          at,
          depth + 1u) ||
        !append_buf_u8(buf, cap, at, 0x0bu)) {
      return 0;
    }
    return 1;
  }
  return 0;
}

static int emit_case_expr_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  if (!span_matches_keyword(env->source, start, end, "case")) {
    return 0;
  }
  uint32_t of_at = find_case_of_at(env->source, start + 4u, end);
  if (of_at >= end) {
    return 0;
  }
  uint32_t targets_start = skip_expr_ws(env->source, start + 4u, of_at);
  uint32_t target_starts[MAX_EVAL_ARGS];
  uint32_t target_ends[MAX_EVAL_ARGS];
  uint32_t target_count = 0u;
  uint32_t cursor = targets_start;
  while (cursor < of_at) {
    if (target_count >= MAX_EVAL_ARGS) {
      return 0;
    }
    uint32_t atom_end = parse_expr_atom_end(env->source, cursor, of_at);
    if (atom_end <= cursor) {
      return 0;
    }
    target_starts[target_count] = cursor;
    target_ends[target_count] = atom_end;
    target_count += 1u;
    cursor = skip_expr_ws(env->source, atom_end, of_at);
  }
  if (target_count == 0u) {
    return 0;
  }
  if (target_count > 1u) {
    uint32_t arms_start = skip_expr_ws(env->source, of_at + 2u, end);
    if (arms_start < end) {
      uint32_t line_end = source_line_end(env->source, arms_start);
      if (line_end > end) {
        line_end = end;
      }
      uint32_t arrow_at = find_case_arm_arrow(env->source, arms_start, line_end);
      if (arrow_at < line_end) {
        uint32_t first_end = parse_expr_atom_end(env->source, arms_start, arrow_at);
        if (first_end > arms_start) {
          uint32_t next = arms_start;
          NameSpan first = parse_simple_name_token(env->source, arms_start, first_end, &next);
          if (first.ok && next == first_end && namespan_starts_with_upper(env->source, first)) {
            target_starts[0] = targets_start;
            target_ends[0] = trim_expr_end(env->source, targets_start, of_at);
            target_count = 1u;
          }
        }
      }
    }
  }
  return emit_case_arms_wasm(
    env,
    target_starts,
    target_ends,
    target_count,
    skip_expr_ws(env->source, of_at + 2u, end),
    end,
    buf,
    cap,
    at,
    depth + 1u
  );
}

static int emit_let_expr_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  if (!span_matches_keyword(env->source, start, end, "let")) {
    return 0;
  }
  uint32_t cursor = start + 3u;
  while (cursor < end) {
    cursor = skip_expr_ws(env->source, cursor, end);
    if (cursor >= end) {
      return 0;
    }
    if (match_keyword_at(env->source, cursor, start, end, "in")) {
      uint32_t in_line_end = source_line_end(env->source, cursor);
      if (in_line_end > end) {
        in_line_end = end;
      }
      return emit_expr_wasm(env, cursor + 2u, in_line_end, buf, cap, at, depth + 1u);
    }
    if (env->local_count >= MAX_PHASE1_EMIT_LOCALS ||
        env->next_local_index >= 0xffffffffu) {
      return 0;
    }
    uint32_t name_next = cursor;
    NameSpan name = parse_simple_name_token(env->source, cursor, end, &name_next);
    uint32_t line_end = source_line_end(env->source, cursor);
    if (line_end > end) {
      line_end = end;
    }
    uint8_t *mem = (uint8_t *) (uintptr_t) env->source.ptr;
    uint32_t eq_at = name_next;
    int ctor_pattern = 0;
    NameSpan ctor_name = missing_name_span();
    uint32_t ctor_pat_arg_starts[MAX_EVAL_ARGS];
    uint32_t ctor_pat_arg_ends[MAX_EVAL_ARGS];
    uint32_t ctor_pat_arg_count = 0u;
    if (name.ok) {
      ctor_name = name;
      uint32_t lookahead = skip_expr_ws(env->source, name_next, line_end);
      if (namespan_starts_with_upper(env->source, name) &&
          lookahead < line_end && mem[lookahead] != '=') {
        ctor_pattern = 1;
        uint32_t pattern_end = lookahead;
        while (pattern_end < line_end && mem[pattern_end] != '=') {
          pattern_end += 1u;
        }
        uint32_t pat_cursor = lookahead;
        while (pat_cursor < pattern_end) {
          uint32_t pat_end = parse_expr_atom_end(env->source, pat_cursor, pattern_end);
          if (pat_end <= pat_cursor) {
            break;
          }
          if (ctor_pat_arg_count >= MAX_EVAL_ARGS) {
            return 0;
          }
          ctor_pat_arg_starts[ctor_pat_arg_count] = pat_cursor;
          ctor_pat_arg_ends[ctor_pat_arg_count] = pat_end;
          ctor_pat_arg_count += 1u;
          pat_cursor = skip_expr_ws(env->source, pat_end, line_end);
        }
      }
    } else {
      uint32_t ctor_next = cursor;
      ctor_name = parse_simple_name_token(env->source, cursor, line_end, &ctor_next);
      if (!ctor_name.ok || !namespan_starts_with_upper(env->source, ctor_name)) {
        return 0;
      }
      uint32_t pat_cursor = skip_expr_ws(env->source, ctor_next, line_end);
      while (pat_cursor < line_end) {
        uint32_t pat_end = parse_expr_atom_end(env->source, pat_cursor, line_end);
        if (pat_end <= pat_cursor) {
          break;
        }
        if (ctor_pat_arg_count >= MAX_EVAL_ARGS) {
          return 0;
        }
        ctor_pat_arg_starts[ctor_pat_arg_count] = pat_cursor;
        ctor_pat_arg_ends[ctor_pat_arg_count] = pat_end;
        ctor_pat_arg_count += 1u;
        pat_cursor = skip_expr_ws(env->source, pat_end, line_end);
      }
      eq_at = ctor_next;
      while (eq_at < line_end && mem[eq_at] != '=') {
        eq_at += 1u;
      }
      ctor_pattern = 1;
    }
    if (!name.ok && !ctor_pattern) {
      return 0;
    }
    while (eq_at < line_end && mem[eq_at] != '=') {
      eq_at += 1u;
    }
    if (eq_at >= line_end) {
      return 0;
    }
    uint32_t value_end = line_end;
    uint32_t next_cursor = line_end;
    int found_in = 0;
    if (!find_let_binding_split(env->source, eq_at + 1u, end, line_end, &value_end, &next_cursor, &found_in)) {
      return 0;
    }
    if (ctor_pattern) {
      NameSpan target_ctor = missing_name_span();
      uint32_t target_arg_starts[MAX_EVAL_ARGS];
      uint32_t target_arg_ends[MAX_EVAL_ARGS];
      uint32_t target_arg_ctor_bindings[MAX_EVAL_ARGS];
      init_ctor_binding_array(target_arg_ctor_bindings, MAX_EVAL_ARGS);
      uint32_t target_arg_count = 0u;
      if (!resolve_direct_constructor_target(
            env,
            eq_at + 1u,
            value_end,
            &target_ctor,
            target_arg_starts,
            target_arg_ends,
            target_arg_ctor_bindings,
            MAX_EVAL_ARGS,
            &target_arg_count,
            0u) ||
          !names_equal(ctor_name, target_ctor) ||
          target_arg_count != ctor_pat_arg_count) {
        return 0;
      }
      for (uint32_t i = 0u; i < ctor_pat_arg_count; i += 1u) {
        uint32_t pat_start = skip_expr_ws(env->source, ctor_pat_arg_starts[i], ctor_pat_arg_ends[i]);
        uint32_t pat_end = trim_expr_end(env->source, pat_start, ctor_pat_arg_ends[i]);
        if (pat_start >= pat_end) {
          return 0;
        }
        if (pat_end == pat_start + 1u && mem[pat_start] == '_') {
          continue;
        }
        uint32_t pat_next = pat_start;
        NameSpan pat_name = parse_simple_name_token(env->source, pat_start, pat_end, &pat_next);
        int bind_ok = 0;
        if (pat_name.ok && pat_next == pat_end) {
          if (target_arg_ctor_bindings[i] != MISSING_CTOR_BINDING) {
            bind_ok = bind_emit_expr_name_with_ctor(
              env,
              pat_name,
              target_arg_starts[i],
              target_arg_ends[i],
              target_arg_ctor_bindings[i]
            );
          } else {
            bind_ok = bind_emit_expr_name(
              env,
              pat_name,
              target_arg_starts[i],
              target_arg_ends[i]
            );
          }
        }
        if (!bind_ok) {
          return 0;
        }
      }
      if (found_in) {
        return emit_expr_wasm(env, next_cursor, line_end, buf, cap, at, depth + 1u);
      }
      cursor = next_cursor;
      continue;
    }
    {
      NameSpan lambda_params[MAX_EVAL_ARGS];
      uint32_t lambda_param_count = 0u;
      uint32_t lambda_body_start = 0u;
      if (parse_lambda_expr(
            env->source,
            eq_at + 1u,
            value_end,
            lambda_params,
            MAX_EVAL_ARGS,
            &lambda_param_count,
            &lambda_body_start)) {
        if (!bind_emit_expr_name(env, name, eq_at + 1u, value_end)) {
          return 0;
        }
        if (found_in) {
          return emit_expr_wasm(env, next_cursor, line_end, buf, cap, at, depth + 1u);
        }
        cursor = next_cursor;
        continue;
      }
    }
    {
      uint32_t ctor_binding = MISSING_CTOR_BINDING;
      if (capture_constructor_binding(env, eq_at + 1u, value_end, &ctor_binding, 0u)) {
        Phase1CtorBinding *binding = &env->ctor_bindings[ctor_binding];
        if (!bind_emit_expr_name_with_ctor(
              env,
              name,
              binding->expr_start,
              binding->expr_end,
              ctor_binding
            )) {
          return 0;
        }
        if (found_in) {
          return emit_expr_wasm(env, next_cursor, line_end, buf, cap, at, depth + 1u);
        }
        cursor = next_cursor;
        continue;
      }
    }
    if (!emit_expr_wasm(env, eq_at + 1u, value_end, buf, cap, at, depth + 1u)) {
      return 0;
    }
    uint32_t local_index = env->next_local_index;
    env->next_local_index += 1u;
    env->local_names[env->local_count] = name;
    env->local_indices[env->local_count] = local_index;
    env->local_count += 1u;
    if (!append_local_set_instr(buf, cap, at, local_index)) {
      return 0;
    }
    if (found_in) {
      return emit_expr_wasm(env, next_cursor, line_end, buf, cap, at, depth + 1u);
    }
    cursor = next_cursor;
  }
  return 0;
}

static int emit_binary_expr_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth,
  const char **ops,
  const uint8_t *opcodes,
  uint32_t op_count
) {
  uint32_t split_at = 0u;
  uint32_t op_len = 0u;
  uint8_t opcode = 0u;
  if (!phase1_find_rightmost_binary_operator(
        env->source,
        start,
        end,
        ops,
        opcodes,
        op_count,
        &split_at,
        &op_len,
        &opcode)) {
    return 0;
  }
  if (!emit_expr_wasm(env, start, split_at, buf, cap, at, depth + 1u)) {
    return 0;
  }
  if (!emit_expr_wasm(env, split_at + op_len, end, buf, cap, at, depth + 1u)) {
    return 0;
  }
  return append_buf_u8(buf, cap, at, opcode);
}

static int emit_expr_wasm(
  Phase1EmitEnv *env,
  uint32_t start,
  uint32_t end,
  uint8_t *buf,
  uint32_t cap,
  uint32_t *at,
  uint32_t depth
) {
  if (depth > 64u) {
    return 0;
  }
  start = skip_expr_ws(env->source, start, end);
  end = trim_expr_end(env->source, start, end);
  if (start >= end) {
    return 0;
  }
  if (span_matches_keyword(env->source, start, end, "let")) {
    uint32_t saved_local_count = env->local_count;
    if (emit_let_expr_wasm(env, start, end, buf, cap, at, depth + 1u)) {
      env->local_count = saved_local_count;
      return 1;
    }
    env->local_count = saved_local_count;
  }
  if (span_matches_keyword(env->source, start, end, "if")) {
    if (emit_if_expr_wasm(env, start, end, buf, cap, at, depth + 1u)) {
      return 1;
    }
  }
  if (span_matches_keyword(env->source, start, end, "case")) {
    if (emit_case_expr_wasm(env, start, end, buf, cap, at, depth + 1u)) {
      return 1;
    }
  }
  if (span_is_wrapped_parens(env->source, start, end)) {
    return emit_expr_wasm(env, start + 1u, end - 1u, buf, cap, at, depth + 1u);
  }
  {
    static const char *bool_ops[] = {"&&", "||"};
    static const uint8_t bool_codes[] = {0x71u, 0x72u};
    if (emit_binary_expr_wasm(env, start, end, buf, cap, at, depth + 1u, bool_ops, bool_codes, 2u)) {
      return 1;
    }
  }
  {
    static const char *cmp_ops[] = {"==", "!=", "<=", ">=", "<", ">"};
    static const uint8_t cmp_codes[] = {0x46u, 0x47u, 0x4cu, 0x4eu, 0x48u, 0x4au};
    if (emit_binary_expr_wasm(env, start, end, buf, cap, at, depth + 1u, cmp_ops, cmp_codes, 6u)) {
      return 1;
    }
  }
  {
    static const char *dotted_ops[] = {"+.", "-.", "*.", "/.", "%."};
    static const uint8_t dotted_codes[] = {0x6au, 0x6bu, 0x6cu, 0x6du, 0x6fu};
    if (emit_binary_expr_wasm(env, start, end, buf, cap, at, depth + 1u, dotted_ops, dotted_codes, 5u)) {
      return 1;
    }
  }
  {
    static const char *add_ops[] = {"+", "-"};
    static const uint8_t add_codes[] = {0x6au, 0x6bu};
    if (emit_binary_expr_wasm(env, start, end, buf, cap, at, depth + 1u, add_ops, add_codes, 2u)) {
      return 1;
    }
  }
  {
    static const char *mul_ops[] = {"*", "/", "%"};
    static const uint8_t mul_codes[] = {0x6cu, 0x6du, 0x6fu};
    if (emit_binary_expr_wasm(env, start, end, buf, cap, at, depth + 1u, mul_ops, mul_codes, 3u)) {
      return 1;
    }
  }
  return emit_prefix_apply_wasm(env, start, end, buf, cap, at, depth + 1u);
}

static Segment build_phase1_dynamic_executable_wasm_base64(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan *roots,
  uint32_t roots_count
) {
  if (roots_count == 0u || roots_have_unknown_names(decls, decl_count, roots, roots_count)) {
    return missing_segment();
  }
  int function_index_by_decl[MAX_FN_DECLS];
  uint32_t reachable_indices[MAX_FN_DECLS];
  uint32_t reachable_count = 0u;
  int *reachable = reachable_workspace;
  int root_decl[MAX_FN_DECLS];
  for (uint32_t i = 0u; i < decl_count; i += 1u) {
    root_decl[i] = 0;
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index >= 0) {
      root_decl[(uint32_t) decl_index] = 1;
    }
  }
  seed_reachable(decls, decl_count, roots, roots_count, reachable);
  expand_reachable(source, decls, decl_count, reachable);
  for (uint32_t i = 0u; i < decl_count; i += 1u) {
    if (!reachable[i] || root_decl[i]) {
      continue;
    }
    if (decl_inline_only_candidate(source, decls, decl_count, i)) {
      reachable[i] = 0;
    }
  }
  for (uint32_t i = 0u; i < decl_count; i += 1u) {
    function_index_by_decl[i] = -1;
    if (!reachable[i]) {
      continue;
    }
    reachable_indices[reachable_count] = i;
    function_index_by_decl[i] = (int) reachable_count;
    reachable_count += 1u;
  }
  if (reachable_count == 0u) {
    return missing_segment();
  }

  uint32_t type_cap = 512u + ((reachable_count + roots_count) * 8u);
  uint32_t func_cap = 512u + ((reachable_count + roots_count) * 8u);
  uint32_t export_cap = 128u + (roots_count * 64u);
  uint32_t code_cap = 4096u + (source.len * 12u) + (reachable_count * 256u) + (roots_count * 32u);
  uint32_t body_cap = 1024u + (source.len * 6u);
  uint32_t module_cap = 4096u + type_cap + func_cap + export_cap + code_cap;
  uint32_t type_ptr = alloc_bytes(type_cap, 1u);
  uint32_t func_ptr = alloc_bytes(func_cap, 1u);
  uint32_t export_ptr = alloc_bytes(export_cap, 1u);
  uint32_t code_ptr = alloc_bytes(code_cap, 1u);
  uint32_t body_ptr = alloc_bytes(body_cap, 1u);
  uint32_t raw_ptr = alloc_bytes(module_cap, 1u);
  if (type_ptr == 0u || func_ptr == 0u || export_ptr == 0u ||
      code_ptr == 0u || body_ptr == 0u || raw_ptr == 0u) {
    return missing_segment();
  }

  uint8_t *type_payload = (uint8_t *) (uintptr_t) type_ptr;
  uint8_t *function_payload = (uint8_t *) (uintptr_t) func_ptr;
  uint8_t *export_payload = (uint8_t *) (uintptr_t) export_ptr;
  uint8_t *code_payload = (uint8_t *) (uintptr_t) code_ptr;
  uint8_t *body_buf = (uint8_t *) (uintptr_t) body_ptr;
  uint8_t *out = (uint8_t *) (uintptr_t) raw_ptr;
  int type_by_arity[MAX_PHASE1_EMIT_TYPES];
  for (uint32_t i = 0u; i < MAX_PHASE1_EMIT_TYPES; i += 1u) {
    type_by_arity[i] = -1;
  }
  uint32_t arity_list[MAX_PHASE1_EMIT_TYPES];
  uint32_t arity_count = 0u;

  uint32_t function_type_indexes[MAX_FN_DECLS + MAX_ROOTS];
  uint32_t total_functions = reachable_count + roots_count;
  for (uint32_t i = 0u; i < reachable_count; i += 1u) {
    uint32_t arity = decl_param_count(source, decls[reachable_indices[i]]);
    if (!ensure_type_for_arity(
          arity,
          type_by_arity,
          arity_list,
          &arity_count,
          &function_type_indexes[i])) {
      return missing_segment();
    }
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index < 0) {
      return missing_segment();
    }
    uint32_t arity = decl_param_count(source, decls[(uint32_t) decl_index]);
    if (!ensure_type_for_arity(
          arity,
          type_by_arity,
          arity_list,
          &arity_count,
          &function_type_indexes[reachable_count + i])) {
      return missing_segment();
    }
  }

  uint32_t type_at = 0u;
  if (!append_buf_var_u32(type_payload, type_cap, &type_at, arity_count)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < arity_count; i += 1u) {
    uint32_t arity = arity_list[i];
    if (!append_buf_u8(type_payload, type_cap, &type_at, 0x60u) ||
        !append_buf_var_u32(type_payload, type_cap, &type_at, arity)) {
      return missing_segment();
    }
    for (uint32_t arg = 0u; arg < arity; arg += 1u) {
      if (!append_buf_u8(type_payload, type_cap, &type_at, 0x7fu)) {
        return missing_segment();
      }
    }
    if (!append_buf_u8(type_payload, type_cap, &type_at, 0x01u) ||
        !append_buf_u8(type_payload, type_cap, &type_at, 0x7fu)) {
      return missing_segment();
    }
  }

  uint32_t function_at = 0u;
  if (!append_buf_var_u32(function_payload, func_cap, &function_at, total_functions)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < total_functions; i += 1u) {
    if (!append_buf_var_u32(function_payload, func_cap, &function_at, function_type_indexes[i])) {
      return missing_segment();
    }
  }

  uint32_t export_at = 0u;
  if (!append_buf_var_u32(export_payload, export_cap, &export_at, roots_count + 1u) ||
      !append_buf_var_u32(export_payload, export_cap, &export_at, 6u)) {
    return missing_segment();
  }
  if (!append_buf_u8(export_payload, export_cap, &export_at, 'm') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'e') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'm') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'o') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'r') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'y') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 0x02u) ||
      !append_buf_u8(export_payload, export_cap, &export_at, 0x00u)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    if (!append_buf_var_u32(export_payload, export_cap, &export_at, roots[i].len)) {
      return missing_segment();
    }
    uint8_t *name_bytes = (uint8_t *) (uintptr_t) roots[i].ptr;
    for (uint32_t j = 0u; j < roots[i].len; j += 1u) {
      if (!append_buf_u8(export_payload, export_cap, &export_at, name_bytes[j])) {
        return missing_segment();
      }
    }
    if (!append_buf_u8(export_payload, export_cap, &export_at, 0x00u) ||
        !append_buf_var_u32(export_payload, export_cap, &export_at, reachable_count + i)) {
      return missing_segment();
    }
  }

  uint32_t code_at = 0u;
  if (!append_buf_var_u32(code_payload, code_cap, &code_at, total_functions)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < reachable_count; i += 1u) {
    FnDecl decl = decls[reachable_indices[i]];
    uint32_t arity = decl_param_count(source, decl);
    uint32_t function_end = decl_function_end(decls, decl_count, reachable_indices[i], source.len);
    Phase1EmitEnv env = {0};
    env.source = source;
    env.decls = decls;
    env.decl_count = decl_count;
    env.function_index_by_decl = function_index_by_decl;
    env.expr_count = 0u;
    env.inline_count = 0u;
    env.ctor_binding_count = 0u;
    env.local_count = 0u;
    env.param_count = arity;
    env.next_local_index = arity;
    NameSpan params[MAX_EVAL_ARGS] = {0};
    uint32_t param_count = collect_decl_params(source, decl, params, MAX_EVAL_ARGS);
    if (param_count != arity || arity > MAX_EVAL_ARGS) {
      return missing_segment();
    }
    for (uint32_t p = 0u; p < arity; p += 1u) {
      env.local_names[env.local_count] = params[p];
      env.local_indices[env.local_count] = p;
      env.local_count += 1u;
    }
    uint32_t body_at = 0u;
    uint32_t expr_end = decl_expression_end(source, decls, decl_count, reachable_indices[i]);
    if (is_simple_clause_block(source, decl, function_end)) {
      if (!emit_clause_decl_wasm(&env, decl, function_end, body_buf, body_cap, &body_at, 0u)) {
        return missing_segment();
      }
    } else if (arity == 0u) {
      EvalConst eval = eval_root_extended(source, decls, decl_count, decl.name);
      if (eval.ok) {
        if (!append_i32_const_instr(body_buf, body_cap, &body_at, eval.value)) {
          return missing_segment();
        }
      } else if (!emit_expr_wasm(&env, decl.body_start, expr_end, body_buf, body_cap, &body_at, 0u)) {
        RawEmitEnv raw_env = {0};
        raw_env.count = 0u;
        raw_env.next_local_index = arity;
        raw_env.function_index_by_decl = function_index_by_decl;
        raw_env.expr_count = 0u;
        raw_env.ctor_binding_count = 0u;
        for (uint32_t p = 0u; p < arity; p += 1u) {
          raw_env.names[raw_env.count] = params[p];
          raw_env.indices[raw_env.count] = p;
          raw_env.count += 1u;
        }
        NameSpan inline_stack[MAX_RAW_EMIT_INLINE_DEPTH] = {0};
        body_at = 0u;
        if (!raw_emit_expr_to_wasm(
              source,
              decls,
              decl_count,
              decl.body_start,
              expr_end,
              &raw_env,
              inline_stack,
              0u,
              body_buf,
              &body_at,
              body_cap)) {
          return missing_segment();
        }
        env.next_local_index = raw_env.next_local_index;
      }
    } else if (!emit_expr_wasm(&env, decl.body_start, expr_end, body_buf, body_cap, &body_at, 0u)) {
      RawEmitEnv raw_env = {0};
      raw_env.count = 0u;
      raw_env.next_local_index = arity;
      raw_env.function_index_by_decl = function_index_by_decl;
      raw_env.expr_count = 0u;
      raw_env.ctor_binding_count = 0u;
      for (uint32_t p = 0u; p < arity; p += 1u) {
        raw_env.names[raw_env.count] = params[p];
        raw_env.indices[raw_env.count] = p;
        raw_env.count += 1u;
      }
      NameSpan inline_stack[MAX_RAW_EMIT_INLINE_DEPTH] = {0};
      body_at = 0u;
      if (!raw_emit_expr_to_wasm(
            source,
            decls,
            decl_count,
            decl.body_start,
            expr_end,
            &raw_env,
            inline_stack,
            0u,
            body_buf,
            &body_at,
            body_cap)) {
        return missing_segment();
      }
      env.next_local_index = raw_env.next_local_index;
    }
    if (!append_buf_u8(body_buf, body_cap, &body_at, 0x0bu)) {
      return missing_segment();
    }
    uint32_t local_decl_at = 0u;
    uint8_t local_decl_bytes[16];
    uint32_t local_count = env.next_local_index > arity ? (env.next_local_index - arity) : 0u;
    if (local_count == 0u) {
      if (!append_buf_u8(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, 0x00u)) {
        return missing_segment();
      }
    } else {
      if (!append_buf_u8(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, 0x01u) ||
          !append_buf_var_u32(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, local_count) ||
          !append_buf_u8(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, 0x7fu)) {
        return missing_segment();
      }
    }
    if (!append_buf_var_u32(code_payload, code_cap, &code_at, local_decl_at + body_at)) {
      return missing_segment();
    }
    for (uint32_t b = 0u; b < local_decl_at; b += 1u) {
      if (!append_buf_u8(code_payload, code_cap, &code_at, local_decl_bytes[b])) {
        return missing_segment();
      }
    }
    for (uint32_t b = 0u; b < body_at; b += 1u) {
      if (!append_buf_u8(code_payload, code_cap, &code_at, body_buf[b])) {
        return missing_segment();
      }
    }
  }

  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    uint32_t body_at = 0u;
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index < 0 || function_index_by_decl[decl_index] < 0) {
      return missing_segment();
    }
    uint32_t arity = decl_param_count(source, decls[(uint32_t) decl_index]);
    for (uint32_t arg = 0u; arg < arity; arg += 1u) {
      if (!append_local_get_instr(body_buf, body_cap, &body_at, arg)) {
        return missing_segment();
      }
    }
    if (!append_call_instr(body_buf, body_cap, &body_at, (uint32_t) function_index_by_decl[decl_index]) ||
        !append_i32_const_instr(body_buf, body_cap, &body_at, 2) ||
        !append_buf_u8(body_buf, body_cap, &body_at, 0x6cu) ||
        !append_i32_const_instr(body_buf, body_cap, &body_at, 1) ||
        !append_buf_u8(body_buf, body_cap, &body_at, 0x6au) ||
        !append_buf_u8(body_buf, body_cap, &body_at, 0x0bu)) {
      return missing_segment();
    }
    if (!append_buf_var_u32(code_payload, code_cap, &code_at, body_at + 1u) ||
        !append_buf_u8(code_payload, code_cap, &code_at, 0x00u)) {
      return missing_segment();
    }
    for (uint32_t b = 0u; b < body_at; b += 1u) {
      if (!append_buf_u8(code_payload, code_cap, &code_at, body_buf[b])) {
        return missing_segment();
      }
    }
  }

  uint32_t cursor = 0u;
  if (!append_buf_u8(out, module_cap, &cursor, 0x00u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x61u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x73u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x6du) ||
      !append_buf_u8(out, module_cap, &cursor, 0x01u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x00u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x00u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x00u)) {
    return missing_segment();
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x01u) ||
      !append_buf_var_u32(out, module_cap, &cursor, type_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < type_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, type_payload[i])) {
      return missing_segment();
    }
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x03u) ||
      !append_buf_var_u32(out, module_cap, &cursor, function_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < function_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, function_payload[i])) {
      return missing_segment();
    }
  }
  {
    uint8_t memory_payload[8];
    uint32_t memory_at = 0u;
    if (!append_buf_var_u32(memory_payload, sizeof(memory_payload), &memory_at, 1u) ||
        !append_buf_u8(memory_payload, sizeof(memory_payload), &memory_at, 0x00u) ||
        !append_buf_var_u32(memory_payload, sizeof(memory_payload), &memory_at, 1u) ||
        !append_buf_u8(out, module_cap, &cursor, 0x05u) ||
        !append_buf_var_u32(out, module_cap, &cursor, memory_at)) {
      return missing_segment();
    }
    for (uint32_t i = 0u; i < memory_at; i += 1u) {
      if (!append_buf_u8(out, module_cap, &cursor, memory_payload[i])) {
        return missing_segment();
      }
    }
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x07u) ||
      !append_buf_var_u32(out, module_cap, &cursor, export_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < export_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, export_payload[i])) {
      return missing_segment();
    }
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x0au) ||
      !append_buf_var_u32(out, module_cap, &cursor, code_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < code_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, code_payload[i])) {
      return missing_segment();
    }
  }

  Segment raw;
  raw.ptr = raw_ptr;
  raw.len = cursor;
  raw.ok = 1;
  return encode_base64_segment(raw);
}

static Segment build_phase1_direct_roots_executable_wasm_base64(
  Segment source,
  FnDecl *decls,
  uint32_t decl_count,
  NameSpan *roots,
  uint32_t roots_count
) {
  if (roots_count == 0u || roots_have_unknown_names(decls, decl_count, roots, roots_count)) {
    return missing_segment();
  }
  int function_index_by_decl[MAX_FN_DECLS];
  for (uint32_t i = 0u; i < decl_count; i += 1u) {
    function_index_by_decl[i] = -1;
  }
  uint32_t type_cap = 512u + (roots_count * 8u);
  uint32_t export_cap = 128u + (roots_count * 64u);
  uint32_t code_cap = 4096u + (source.len * 12u) + (roots_count * 256u);
  uint32_t body_cap = 1024u + (source.len * 6u);
  uint32_t module_cap = 4096u + type_cap + export_cap + code_cap;
  uint32_t type_ptr = alloc_bytes(type_cap, 1u);
  uint32_t export_ptr = alloc_bytes(export_cap, 1u);
  uint32_t code_ptr = alloc_bytes(code_cap, 1u);
  uint32_t body_ptr = alloc_bytes(body_cap, 1u);
  uint32_t raw_ptr = alloc_bytes(module_cap, 1u);
  if (type_ptr == 0u || export_ptr == 0u || code_ptr == 0u ||
      body_ptr == 0u || raw_ptr == 0u) {
    return missing_segment();
  }

  uint8_t *type_payload = (uint8_t *) (uintptr_t) type_ptr;
  uint8_t *export_payload = (uint8_t *) (uintptr_t) export_ptr;
  uint8_t *code_payload = (uint8_t *) (uintptr_t) code_ptr;
  uint8_t *body_buf = (uint8_t *) (uintptr_t) body_ptr;
  uint8_t *out = (uint8_t *) (uintptr_t) raw_ptr;
  int type_by_arity[MAX_PHASE1_EMIT_TYPES];
  uint32_t arity_list[MAX_PHASE1_EMIT_TYPES];
  uint32_t arity_count = 0u;
  uint32_t function_type_indexes[MAX_ROOTS];
  for (uint32_t i = 0u; i < MAX_PHASE1_EMIT_TYPES; i += 1u) {
    type_by_arity[i] = -1;
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index < 0) {
      return missing_segment();
    }
    uint32_t arity = decl_param_count(source, decls[(uint32_t) decl_index]);
    if (!ensure_type_for_arity(
          arity,
          type_by_arity,
          arity_list,
          &arity_count,
          &function_type_indexes[i])) {
      return missing_segment();
    }
  }

  uint32_t type_at = 0u;
  if (!append_buf_var_u32(type_payload, type_cap, &type_at, arity_count)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < arity_count; i += 1u) {
    uint32_t arity = arity_list[i];
    if (!append_buf_u8(type_payload, type_cap, &type_at, 0x60u) ||
        !append_buf_var_u32(type_payload, type_cap, &type_at, arity)) {
      return missing_segment();
    }
    for (uint32_t arg = 0u; arg < arity; arg += 1u) {
      if (!append_buf_u8(type_payload, type_cap, &type_at, 0x7fu)) {
        return missing_segment();
      }
    }
    if (!append_buf_u8(type_payload, type_cap, &type_at, 0x01u) ||
        !append_buf_u8(type_payload, type_cap, &type_at, 0x7fu)) {
      return missing_segment();
    }
  }

  uint8_t function_payload[512];
  uint32_t function_at = 0u;
  if (!append_buf_var_u32(function_payload, sizeof(function_payload), &function_at, roots_count)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    if (!append_buf_var_u32(function_payload, sizeof(function_payload), &function_at, function_type_indexes[i])) {
      return missing_segment();
    }
  }

  uint32_t export_at = 0u;
  if (!append_buf_var_u32(export_payload, export_cap, &export_at, roots_count + 1u) ||
      !append_buf_var_u32(export_payload, export_cap, &export_at, 6u)) {
    return missing_segment();
  }
  if (!append_buf_u8(export_payload, export_cap, &export_at, 'm') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'e') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'm') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'o') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'r') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 'y') ||
      !append_buf_u8(export_payload, export_cap, &export_at, 0x02u) ||
      !append_buf_u8(export_payload, export_cap, &export_at, 0x00u)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    if (!append_buf_var_u32(export_payload, export_cap, &export_at, roots[i].len)) {
      return missing_segment();
    }
    uint8_t *name_bytes = (uint8_t *) (uintptr_t) roots[i].ptr;
    for (uint32_t j = 0u; j < roots[i].len; j += 1u) {
      if (!append_buf_u8(export_payload, export_cap, &export_at, name_bytes[j])) {
        return missing_segment();
      }
    }
    if (!append_buf_u8(export_payload, export_cap, &export_at, 0x00u) ||
        !append_buf_var_u32(export_payload, export_cap, &export_at, i)) {
      return missing_segment();
    }
  }

  uint32_t code_at = 0u;
  if (!append_buf_var_u32(code_payload, code_cap, &code_at, roots_count)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index < 0) {
      return missing_segment();
    }
    FnDecl decl = decls[(uint32_t) decl_index];
    uint32_t arity = decl_param_count(source, decl);
    Phase1EmitEnv env = {0};
    env.source = source;
    env.decls = decls;
    env.decl_count = decl_count;
    env.function_index_by_decl = function_index_by_decl;
    env.expr_count = 0u;
    env.inline_count = 0u;
    env.ctor_binding_count = 0u;
    env.local_count = 0u;
    env.param_count = arity;
    env.next_local_index = arity;
    NameSpan params[MAX_EVAL_ARGS] = {0};
    uint32_t param_count = collect_decl_params(source, decl, params, MAX_EVAL_ARGS);
    if (param_count != arity || arity > MAX_EVAL_ARGS) {
      return missing_segment();
    }
    for (uint32_t p = 0u; p < arity; p += 1u) {
      env.local_names[env.local_count] = params[p];
      env.local_indices[env.local_count] = p;
      env.local_count += 1u;
    }
    uint32_t body_at = 0u;
    uint32_t expr_end = decl_expression_end(source, decls, decl_count, (uint32_t) decl_index);
    if (!emit_expr_wasm(&env, decl.body_start, expr_end, body_buf, body_cap, &body_at, 0u)) {
      RawEmitEnv raw_env = {0};
      raw_env.count = 0u;
      raw_env.next_local_index = arity;
      raw_env.function_index_by_decl = function_index_by_decl;
      raw_env.expr_count = 0u;
      raw_env.ctor_binding_count = 0u;
      for (uint32_t p = 0u; p < arity; p += 1u) {
        raw_env.names[raw_env.count] = params[p];
        raw_env.indices[raw_env.count] = p;
        raw_env.count += 1u;
      }
      NameSpan inline_stack[MAX_RAW_EMIT_INLINE_DEPTH] = {0};
      body_at = 0u;
      if (!raw_emit_expr_to_wasm(
            source,
            decls,
            decl_count,
            decl.body_start,
            expr_end,
            &raw_env,
            inline_stack,
            0u,
            body_buf,
            &body_at,
            body_cap)) {
        return missing_segment();
      }
      env.next_local_index = raw_env.next_local_index;
    }
    if (!append_i32_const_instr(body_buf, body_cap, &body_at, 2) ||
        !append_buf_u8(body_buf, body_cap, &body_at, 0x6cu) ||
        !append_i32_const_instr(body_buf, body_cap, &body_at, 1) ||
        !append_buf_u8(body_buf, body_cap, &body_at, 0x6au) ||
        !append_buf_u8(body_buf, body_cap, &body_at, 0x0bu)) {
      return missing_segment();
    }
    uint32_t local_decl_at = 0u;
    uint8_t local_decl_bytes[16];
    uint32_t local_count = env.next_local_index > arity ? (env.next_local_index - arity) : 0u;
    if (local_count == 0u) {
      if (!append_buf_u8(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, 0x00u)) {
        return missing_segment();
      }
    } else {
      if (!append_buf_u8(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, 0x01u) ||
          !append_buf_var_u32(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, local_count) ||
          !append_buf_u8(local_decl_bytes, sizeof(local_decl_bytes), &local_decl_at, 0x7fu)) {
        return missing_segment();
      }
    }
    if (!append_buf_var_u32(code_payload, code_cap, &code_at, local_decl_at + body_at)) {
      return missing_segment();
    }
    for (uint32_t b = 0u; b < local_decl_at; b += 1u) {
      if (!append_buf_u8(code_payload, code_cap, &code_at, local_decl_bytes[b])) {
        return missing_segment();
      }
    }
    for (uint32_t b = 0u; b < body_at; b += 1u) {
      if (!append_buf_u8(code_payload, code_cap, &code_at, body_buf[b])) {
        return missing_segment();
      }
    }
  }

  uint32_t cursor = 0u;
  if (!append_buf_u8(out, module_cap, &cursor, 0x00u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x61u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x73u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x6du) ||
      !append_buf_u8(out, module_cap, &cursor, 0x01u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x00u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x00u) ||
      !append_buf_u8(out, module_cap, &cursor, 0x00u)) {
    return missing_segment();
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x01u) ||
      !append_buf_var_u32(out, module_cap, &cursor, type_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < type_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, type_payload[i])) {
      return missing_segment();
    }
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x03u) ||
      !append_buf_var_u32(out, module_cap, &cursor, function_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < function_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, function_payload[i])) {
      return missing_segment();
    }
  }
  {
    uint8_t memory_payload[8];
    uint32_t memory_at = 0u;
    if (!append_buf_var_u32(memory_payload, sizeof(memory_payload), &memory_at, 1u) ||
        !append_buf_u8(memory_payload, sizeof(memory_payload), &memory_at, 0x00u) ||
        !append_buf_var_u32(memory_payload, sizeof(memory_payload), &memory_at, 1u) ||
        !append_buf_u8(out, module_cap, &cursor, 0x05u) ||
        !append_buf_var_u32(out, module_cap, &cursor, memory_at)) {
      return missing_segment();
    }
    for (uint32_t i = 0u; i < memory_at; i += 1u) {
      if (!append_buf_u8(out, module_cap, &cursor, memory_payload[i])) {
        return missing_segment();
      }
    }
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x07u) ||
      !append_buf_var_u32(out, module_cap, &cursor, export_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < export_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, export_payload[i])) {
      return missing_segment();
    }
  }
  if (!append_buf_u8(out, module_cap, &cursor, 0x0au) ||
      !append_buf_var_u32(out, module_cap, &cursor, code_at)) {
    return missing_segment();
  }
  for (uint32_t i = 0u; i < code_at; i += 1u) {
    if (!append_buf_u8(out, module_cap, &cursor, code_payload[i])) {
      return missing_segment();
    }
  }
  {
    Segment raw;
    raw.ptr = raw_ptr;
    raw.len = cursor;
    raw.ok = 1;
    return encode_base64_segment(raw);
  }
}

static uint32_t collect_export_roots_from_source(Segment source, NameSpan *roots, uint32_t roots_count) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t line_start = 0u;
  while (line_start < source.len) {
    uint32_t line_end = source_line_end(source, line_start);
    line_end = extend_lambda_truncated_line_end(source, line_start, line_end);
    uint32_t next_line = source_next_line_start(source, line_end);
    uint32_t trimmed = source_skip_line_ws(source, line_start, line_end);
    if (trimmed == line_start &&
        trimmed + 6u <= line_end &&
        mem[trimmed + 0u] == 'e' &&
        mem[trimmed + 1u] == 'x' &&
        mem[trimmed + 2u] == 'p' &&
        mem[trimmed + 3u] == 'o' &&
        mem[trimmed + 4u] == 'r' &&
        mem[trimmed + 5u] == 't') {
      uint32_t at = trimmed + 6u;
      while (at < line_end) {
        while (at < line_end &&
               (mem[at] == ' ' || mem[at] == '\t' || mem[at] == ',')) {
          at += 1u;
        }
        if (at >= line_end) {
          break;
        }
        uint32_t name_end = at;
        uint8_t b0 = mem[at];
        if (is_ident_start(b0)) {
          name_end = source_parse_ident_end(source, at, line_end);
        } else if (is_operator_start(b0)) {
          name_end = source_parse_operator_end(source, at, line_end);
        } else {
          at += 1u;
          continue;
        }
        if (name_end <= at) {
          at += 1u;
          continue;
        }
        NameSpan root;
        root.ptr = source.ptr + at;
        root.len = name_end - at;
        root.ok = root.len > 0u;
        if (root.ok && (!is_ident_start(b0) || !is_keyword_name(root))) {
          roots_count = roots_push_unique(root, roots, roots_count);
        }
        at = name_end;
      }
    }
    line_start = next_line;
  }
  return roots_count;
}

static uint32_t json_string_end(uint32_t req_ptr, uint32_t at, uint32_t req_len) {
  uint8_t *req = (uint8_t *) (uintptr_t) req_ptr;
  uint32_t i = at + 1u;
  int escaped = 0;
  while (i < req_len) {
    uint8_t c = req[i];
    if (escaped) {
      escaped = 0;
      i += 1u;
      continue;
    }
    if (c == '\\') {
      escaped = 1;
      i += 1u;
      continue;
    }
    if (c == '"') {
      return i + 1u;
    }
    i += 1u;
  }
  return req_len;
}

static uint32_t json_key_value_end_top_level(uint32_t req_ptr, uint32_t req_len, const char *key) {
  uint8_t *req = (uint8_t *) (uintptr_t) req_ptr;
  uint32_t key_len = cstr_len(key);
  uint32_t depth = 0u;
  uint32_t i = 0u;
  while (i < req_len) {
    uint8_t c = req[i];
    if (c == '"') {
      if (depth == 1u) {
        int matches = 1;
        if (i + key_len <= req_len) {
          for (uint32_t k = 0u; k < key_len; k += 1u) {
            if (req[i + k] != (uint8_t) key[k]) {
              matches = 0;
              break;
            }
          }
        } else {
          matches = 0;
        }
        if (matches) {
          return i + key_len;
        }
      }
      i = json_string_end(req_ptr, i, req_len);
      continue;
    }
    if (c == '{') {
      depth += 1u;
    } else if (c == '}') {
      if (depth > 0u) {
        depth -= 1u;
      }
    }
    i += 1u;
  }
  return req_len;
}

static int collect_entrypoint_roots_from_request(
  uint32_t req_ptr,
  uint32_t req_len,
  NameSpan *roots,
  uint32_t *roots_count,
  int *saw_invalid_root
) {
  const char *key = "\"entrypoint_exports\"";
  uint32_t at = json_key_value_end_top_level(req_ptr, req_len, key);
  if (at == req_len) {
    return 0;
  }
  uint8_t *req = (uint8_t *) (uintptr_t) req_ptr;
  uint32_t i = at;
  while (i < req_len && req[i] != ':') {
    i += 1u;
  }
  if (i >= req_len) {
    return 0;
  }
  i += 1u;
  while (i < req_len && is_ws(req[i])) {
    i += 1u;
  }
  if (i >= req_len || req[i] != '[') {
    return 0;
  }
  i += 1u;
  *saw_invalid_root = 0;
  uint32_t before = *roots_count;
  while (i < req_len) {
    while (i < req_len && (is_ws(req[i]) || req[i] == ',')) {
      i += 1u;
    }
    if (i >= req_len || req[i] == ']') {
      break;
    }
    if (req[i] != '"') {
      i += 1u;
      continue;
    }
    i += 1u;
    uint32_t name_start = i;
    int escaped = 0;
    while (i < req_len) {
      uint8_t c = req[i];
      if (escaped) {
        escaped = 0;
        i += 1u;
        continue;
      }
      if (c == '\\') {
        escaped = 1;
        i += 1u;
        continue;
      }
      if (c == '"') {
        break;
      }
      i += 1u;
    }
    if (i <= req_len) {
      NameSpan root;
      root.ptr = req_ptr + name_start;
      root.len = i - name_start;
      root.ok = root.len > 0u;
      if (is_root_name_span_valid(root)) {
        *roots_count = roots_push_unique(root, roots, *roots_count);
      } else {
        *saw_invalid_root = 1;
      }
    }
    if (i < req_len && req[i] == '"') {
      i += 1u;
    }
  }
  return *roots_count > before;
}

static uint32_t collect_fn_decls(Segment source, FnDecl *decls, uint32_t max_decls) {
  uint32_t count = 0u;
  uint32_t line_start = 0u;
  while (line_start < source.len) {
    uint32_t line_end = source_line_end(source, line_start);
    line_end = extend_lambda_truncated_line_end(source, line_start, line_end);
    uint32_t next_line = source_next_line_start(source, line_end);
    if (count < max_decls) {
      FnDecl decl;
      if (parse_top_level_decl(source, line_start, line_end, next_line, &decl)) {
        decls[count] = decl;
        count += 1u;
      }
    }
    line_start = next_line;
  }
  return count;
}

static void seed_reachable(FnDecl *decls, uint32_t decl_count, NameSpan *roots, uint32_t roots_count, int *reachable) {
  for (uint32_t i = 0; i < decl_count; i += 1u) {
    reachable[i] = 0;
  }
  for (uint32_t r = 0; r < roots_count; r += 1u) {
    for (uint32_t i = 0; i < decl_count; i += 1u) {
      if (names_equal(roots[r], decls[i].name)) {
        reachable[i] = 1;
      }
    }
  }
}

static int roots_have_unknown_names(FnDecl *decls, uint32_t decl_count, NameSpan *roots, uint32_t roots_count) {
  for (uint32_t r = 0u; r < roots_count; r += 1u) {
    int found = 0;
    for (uint32_t i = 0u; i < decl_count; i += 1u) {
      if (names_equal(roots[r], decls[i].name)) {
        found = 1;
        break;
      }
    }
    if (!found) {
      return 1;
    }
  }
  return 0;
}

static void expand_reachable(Segment source, FnDecl *decls, uint32_t decl_count, int *reachable) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  int changed = 1;
  while (changed) {
    changed = 0;
    for (uint32_t i = 0; i < decl_count; i += 1u) {
      if (!reachable[i]) {
        continue;
      }
      uint32_t expr_end = decl_expression_end(source, decls, decl_count, i);
      uint32_t at = decls[i].body_start;
      while (at < expr_end) {
        if (mem[at] == '-' && at + 1u < expr_end && mem[at + 1u] == '-') {
          at = source_line_end(source, at);
          continue;
        }
        if (mem[at] == '"') {
          uint32_t next = json_string_end(source.ptr, at, expr_end);
          at = next > at ? next : at + 1u;
          continue;
        }
        uint32_t tok_end = at;
        if (is_ident_start(mem[at])) {
          tok_end = source_parse_ident_end(source, at, expr_end);
        } else if (is_operator_start(mem[at])) {
          tok_end = source_parse_operator_end(source, at, expr_end);
        } else {
          at += 1u;
          continue;
        }
        if (tok_end <= at) {
          at += 1u;
          continue;
        }
        NameSpan tok;
        tok.ptr = source.ptr + at;
        tok.len = tok_end - at;
        tok.ok = tok.len > 0u;
        if (tok.ok) {
          for (uint32_t k = 0; k < decl_count; k += 1u) {
            if (!reachable[k] && names_equal(tok, decls[k].name)) {
              reachable[k] = 1;
              changed = 1;
            }
          }
        }
        at = tok_end;
      }
    }
  }
}

static Segment build_pruned_segment(Segment source, FnDecl *decls, uint32_t decl_count, int *reachable) {
  uint32_t out_ptr = alloc_bytes(source.len, 1u);
  if (out_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *src = (uint8_t *) (uintptr_t) source.ptr;
  uint8_t *dst = (uint8_t *) (uintptr_t) out_ptr;
  uint32_t cursor = 0u;
  uint32_t line_start = 0u;
  uint32_t decl_index = 0u;
  while (line_start < source.len) {
    uint32_t line_end = source_line_end(source, line_start);
    uint32_t next_line = source_next_line_start(source, line_end);
    int keep_line = 1;
    if (decl_index < decl_count && decls[decl_index].line_start == line_start) {
      keep_line = reachable[decl_index];
      if (!keep_line) {
        line_start = (decl_index + 1u < decl_count) ? decls[decl_index + 1u].line_start : source.len;
        decl_index += 1u;
        continue;
      }
      decl_index += 1u;
    }
    if (keep_line) {
      for (uint32_t i = line_start; i < next_line; i += 1u) {
        dst[cursor] = src[i];
        cursor += 1u;
      }
    }
    line_start = next_line;
  }
  Segment out;
  out.ptr = out_ptr;
  out.len = cursor;
  out.ok = 1;
  return out;
}

static Segment build_collapsed_segment(Segment source, FnDecl *decls, uint32_t decl_count) {
  if (decl_count == 0u) {
    return source;
  }
  int *tail_target = tail_target_workspace;
  int *self_tail = self_tail_workspace;
  int *mutual_tail = mutual_tail_workspace;
  for (uint32_t i = 0; i < decl_count; i += 1u) {
    tail_target[i] = -1;
    self_tail[i] = 0;
    mutual_tail[i] = 0;
  }
  for (uint32_t i = 0; i < decl_count; i += 1u) {
    NameSpan head = parse_body_head_call_name(source, decls[i]);
    if (!head.ok) {
      continue;
    }
    int target = find_decl_index_by_name(decls, decl_count, head);
    if (target < 0) {
      continue;
    }
    tail_target[i] = target;
    if ((uint32_t) target == i) {
      self_tail[i] = 1;
    }
  }
  for (uint32_t i = 0; i < decl_count; i += 1u) {
    if (tail_target[i] < 0 || (uint32_t) tail_target[i] == i) {
      continue;
    }
    int target = tail_target[i];
    if (target >= 0 && (uint32_t) target < decl_count &&
        tail_target[target] == (int) i) {
      mutual_tail[i] = 1;
    }
  }

  int has_markers = 0;
  uint32_t total_len = source.len;
  for (uint32_t i = 0; i < decl_count; i += 1u) {
    if (self_tail[i]) {
      has_markers = 1;
      total_len += cstr_len(TAIL_SELF_PREFIX) + decls[i].name.len;
    }
    if (mutual_tail[i]) {
      int target = tail_target[i];
      if (target >= 0 && (uint32_t) target < decl_count) {
        has_markers = 1;
        total_len += cstr_len(TAIL_MUTUAL_PREFIX) + decls[i].name.len +
          cstr_len(TAIL_ARROW) + decls[(uint32_t) target].name.len;
      }
    }
  }
  if (!has_markers) {
    return source;
  }

  uint32_t out_ptr = alloc_bytes(total_len, 1u);
  if (out_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) out_ptr;
  uint8_t *src = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t cursor = 0u;
  for (uint32_t i = 0; i < source.len; i += 1u) {
    dst[cursor] = src[i];
    cursor += 1u;
  }
  for (uint32_t i = 0; i < decl_count; i += 1u) {
    if (self_tail[i]) {
      write_literal(dst, &cursor, TAIL_SELF_PREFIX);
      write_name_span(dst, &cursor, decls[i].name);
    }
    if (mutual_tail[i]) {
      int target = tail_target[i];
      if (target >= 0 && (uint32_t) target < decl_count) {
        write_literal(dst, &cursor, TAIL_MUTUAL_PREFIX);
        write_name_span(dst, &cursor, decls[i].name);
        write_literal(dst, &cursor, TAIL_ARROW);
        write_name_span(dst, &cursor, decls[(uint32_t) target].name);
      }
    }
  }
  Segment out;
  out.ptr = out_ptr;
  out.len = cursor;
  out.ok = 1;
  return out;
}

static Segment prune_compile_source(
  uint32_t req_ptr,
  uint32_t req_len,
  Segment source_seg,
  int *has_entrypoint_override_out,
  int enable_request_shape_pruning,
  const char **error_out
) {
  NameSpan *roots = roots_workspace;
  uint32_t roots_count = 0u;
  int saw_invalid_root = 0;
  *error_out = 0;
  *has_entrypoint_override_out = collect_entrypoint_roots_from_request(
    req_ptr,
    req_len,
    roots,
    &roots_count,
    &saw_invalid_root
  );
  if (*has_entrypoint_override_out && (saw_invalid_root || roots_count == 0u)) {
    *error_out = ENTRYPOINT_ROOT_INVALID_ERROR;
    return missing_segment();
  }
  if (!*has_entrypoint_override_out && !enable_request_shape_pruning) {
    return source_seg;
  }
  if (roots_count == 0u) {
    roots_count = collect_export_roots_from_source(source_seg, roots, roots_count);
  }
  if (roots_count == 0u) {
    NameSpan fallback_root;
    fallback_root.ptr = (uint32_t) (uintptr_t) "main";
    fallback_root.len = 4u;
    fallback_root.ok = 1;
    roots_count = roots_push_unique(fallback_root, roots, roots_count);
  }

  FnDecl *decls = fn_decls_workspace;
  int *reachable = reachable_workspace;
  uint32_t decl_count = collect_fn_decls(source_seg, decls, MAX_FN_DECLS);
  if (decl_count == 0u) {
    if (*has_entrypoint_override_out) {
      *error_out = ENTRYPOINT_ROOT_UNKNOWN_ERROR;
      return missing_segment();
    }
    return source_seg;
  }
  if (*has_entrypoint_override_out &&
      roots_have_unknown_names(decls, decl_count, roots, roots_count)) {
    *error_out = ENTRYPOINT_ROOT_UNKNOWN_ERROR;
    return missing_segment();
  }
  seed_reachable(decls, decl_count, roots, roots_count, reachable);
  expand_reachable(source_seg, decls, decl_count, reachable);
  Segment pruned_source = build_pruned_segment(source_seg, decls, decl_count, reachable);
  if (!pruned_source.ok) {
    return missing_segment();
  }
  if (!has_entrypoint_override_out || !*has_entrypoint_override_out) {
    return pruned_source;
  }
  Segment temp_pruned_source = build_temp_pruned_segment(pruned_source);
  if (!temp_pruned_source.ok) {
    return missing_segment();
  }
  return temp_pruned_source;
}

static uint32_t collect_effective_compile_roots(
  uint32_t req_ptr,
  uint32_t req_len,
  Segment source_seg,
  NameSpan *roots
) {
  uint32_t roots_count = 0u;
  int saw_invalid_root = 0;
  int has_override = collect_entrypoint_roots_from_request(
    req_ptr,
    req_len,
    roots,
    &roots_count,
    &saw_invalid_root
  );
  if (has_override && !saw_invalid_root && roots_count > 0u) {
    return roots_count;
  }
  if (roots_count == 0u) {
    roots_count = collect_export_roots_from_source(source_seg, roots, roots_count);
  }
  if (roots_count == 0u) {
    NameSpan fallback_root;
    fallback_root.ptr = (uint32_t) (uintptr_t) "main";
    fallback_root.len = 4u;
    fallback_root.ok = 1;
    roots_count = roots_push_unique(fallback_root, roots, roots_count);
  }
  return roots_count;
}

static Segment build_compile_public_exports_json(
  Segment source_seg,
  NameSpan *roots,
  uint32_t roots_count
) {
  FnDecl *decls = fn_decls_workspace;
  uint32_t decl_count = collect_fn_decls(source_seg, decls, MAX_FN_DECLS);
  uint32_t out_len = 2u;
  for (uint32_t i = 0; i < roots_count; i += 1u) {
    uint32_t arity = 0u;
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index >= 0) {
      arity = decl_param_count(source_seg, decls[(uint32_t) decl_index]);
    }
    if (i > 0u) {
      out_len += 1u;
    }
    out_len += cstr_len("{\"name\":\"");
    out_len += name_span_json_escaped_len(roots[i]);
    out_len += cstr_len("\",\"arity\":");
    if (arity >= 10u) {
      out_len += 1u;
    }
    if (arity >= 100u) {
      out_len += 1u;
    }
    out_len += 2u;
  }
  uint32_t out_ptr = alloc_bytes(out_len, 1u);
  if (out_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) out_ptr;
  uint32_t cursor = 0u;
  dst[cursor++] = '[';
  for (uint32_t i = 0; i < roots_count; i += 1u) {
    uint32_t arity = 0u;
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index >= 0) {
      arity = decl_param_count(source_seg, decls[(uint32_t) decl_index]);
    }
    if (i > 0u) {
      dst[cursor++] = ',';
    }
    write_literal(dst, &cursor, "{\"name\":\"");
    write_json_escaped_name_span(dst, &cursor, roots[i]);
    write_literal(dst, &cursor, "\",\"arity\":");
    if (arity >= 100u) {
      dst[cursor++] = (uint8_t) ('0' + ((arity / 100u) % 10u));
    }
    if (arity >= 10u) {
      dst[cursor++] = (uint8_t) ('0' + ((arity / 10u) % 10u));
    }
    dst[cursor++] = (uint8_t) ('0' + (arity % 10u));
    dst[cursor++] = '}';
  }
  dst[cursor++] = ']';
  Segment out;
  out.ptr = out_ptr;
  out.len = cursor;
  out.ok = 1;
  return out;
}

static Segment build_compile_dts(
  Segment source_seg,
  NameSpan *roots,
  uint32_t roots_count
) {
  FnDecl *decls = fn_decls_workspace;
  uint32_t decl_count = collect_fn_decls(source_seg, decls, MAX_FN_DECLS);
  uint32_t out_len = 0u;
  for (uint32_t i = 0; i < roots_count; i += 1u) {
    uint32_t arity = 0u;
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index >= 0) {
      arity = decl_param_count(source_seg, decls[(uint32_t) decl_index]);
    }
    out_len += cstr_len("export declare function ");
    out_len += name_span_json_escaped_len(roots[i]);
    out_len += cstr_len("(): number;\\n");
    for (uint32_t arg = 0u; arg < arity; arg += 1u) {
      if (arg > 0u) {
        out_len += cstr_len(", ");
      }
      out_len += cstr_len("arg0: number");
      if (arg >= 10u) {
        out_len += 1u;
      }
      if (arg >= 100u) {
        out_len += 1u;
      }
    }
  }
  uint32_t out_ptr = alloc_bytes(out_len == 0u ? 1u : out_len, 1u);
  if (out_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) out_ptr;
  uint32_t cursor = 0u;
  for (uint32_t i = 0; i < roots_count; i += 1u) {
    uint32_t arity = 0u;
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index >= 0) {
      arity = decl_param_count(source_seg, decls[(uint32_t) decl_index]);
    }
    write_literal(dst, &cursor, "export declare function ");
    write_json_escaped_name_span(dst, &cursor, roots[i]);
    dst[cursor++] = '(';
    for (uint32_t arg = 0u; arg < arity; arg += 1u) {
      if (arg > 0u) {
        write_literal(dst, &cursor, ", ");
      }
      write_literal(dst, &cursor, "arg");
      if (arg >= 100u) {
        dst[cursor++] = (uint8_t) ('0' + ((arg / 100u) % 10u));
      }
      if (arg >= 10u) {
        dst[cursor++] = (uint8_t) ('0' + ((arg / 10u) % 10u));
      }
      dst[cursor++] = (uint8_t) ('0' + (arg % 10u));
      write_literal(dst, &cursor, ": number");
    }
    write_literal(dst, &cursor, "): number;\\n");
  }
  Segment out;
  out.ptr = out_ptr;
  out.len = cursor;
  out.ok = 1;
  return out;
}

static Segment build_phase1_dynamic_stub_wasm_base64(
  Segment source_seg,
  NameSpan *roots,
  uint32_t roots_count
) {
  Segment eval_source = decode_json_source_segment(source_seg);
  if (!eval_source.ok) {
    eval_source = source_seg;
  }
  FnDecl *decls = fn_decls_workspace;
  uint32_t decl_count = collect_fn_decls(eval_source, decls, MAX_FN_DECLS);
  {
    Segment executable = build_phase1_dynamic_executable_wasm_base64(
      eval_source,
      decls,
      decl_count,
      roots,
      roots_count
    );
    if (executable.ok) {
      return executable;
    }
  }
  {
    Segment direct_roots = build_phase1_direct_roots_executable_wasm_base64(
      eval_source,
      decls,
      decl_count,
      roots,
      roots_count
    );
    if (direct_roots.ok) {
      return direct_roots;
    }
  }
  uint32_t raw_ptr = alloc_bytes(256u + (roots_count * 96u), 1u);
  if (raw_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *out = (uint8_t *) (uintptr_t) raw_ptr;
  uint32_t cursor = 0u;
  out[cursor++] = 0x00u;
  out[cursor++] = 0x61u;
  out[cursor++] = 0x73u;
  out[cursor++] = 0x6du;
  out[cursor++] = 0x01u;
  out[cursor++] = 0x00u;
  out[cursor++] = 0x00u;
  out[cursor++] = 0x00u;

  uint8_t type_payload[1024];
  uint32_t type_at = 0u;
  type_at = append_var_u32(type_payload, type_at, roots_count);
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    uint32_t arity = 0u;
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index >= 0) {
      arity = decl_param_count(eval_source, decls[(uint32_t) decl_index]);
    }
    type_payload[type_at++] = 0x60u;
    type_at = append_var_u32(type_payload, type_at, arity);
    for (uint32_t arg = 0u; arg < arity; arg += 1u) {
      type_payload[type_at++] = 0x7fu;
    }
    type_payload[type_at++] = 0x01u;
    type_payload[type_at++] = 0x7fu;
  }
  out[cursor++] = 0x01u;
  cursor = append_var_u32(out, cursor, type_at);
  for (uint32_t i = 0u; i < type_at; i += 1u) {
    out[cursor++] = type_payload[i];
  }

  uint8_t function_payload[512];
  uint32_t function_at = 0u;
  function_at = append_var_u32(function_payload, function_at, roots_count);
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    function_at = append_var_u32(function_payload, function_at, i);
  }
  out[cursor++] = 0x03u;
  cursor = append_var_u32(out, cursor, function_at);
  for (uint32_t i = 0u; i < function_at; i += 1u) {
    out[cursor++] = function_payload[i];
  }

  uint8_t memory_payload[8];
  uint32_t memory_at = 0u;
  memory_at = append_var_u32(memory_payload, memory_at, 1u);
  memory_payload[memory_at++] = 0x00u;
  memory_at = append_var_u32(memory_payload, memory_at, 1u);
  out[cursor++] = 0x05u;
  cursor = append_var_u32(out, cursor, memory_at);
  for (uint32_t i = 0u; i < memory_at; i += 1u) {
    out[cursor++] = memory_payload[i];
  }

  uint32_t root_value_ptr = alloc_bytes(roots_count * sizeof(EvalValue), 4u);
  if (root_value_ptr == 0u) {
    return missing_segment();
  }
  EvalValue *root_values = (EvalValue *) (uintptr_t) root_value_ptr;
  uint32_t slice_data_bytes = 0u;
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    root_values[i] = missing_eval_value();
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    if (decl_index < 0) {
      continue;
    }
    if (decl_param_count(eval_source, decls[(uint32_t) decl_index]) != 0u) {
      continue;
    }
    root_values[i] = eval_decl_by_name_extended(
      eval_source,
      decls,
      decl_count,
      roots[i],
      NULL,
      0u,
      0u
    );
    if (root_values[i].ok && root_values[i].kind == EVAL_VALUE_SLICE) {
      slice_data_bytes += 8u + root_values[i].slice_len;
    }
  }

  uint32_t export_ptr = alloc_bytes(64u + (roots_count * 64u), 1u);
  if (export_ptr == 0u) {
    return missing_segment();
  }
  uint8_t *export_payload = (uint8_t *) (uintptr_t) export_ptr;
  uint32_t export_at = 0u;
  export_at = append_var_u32(export_payload, export_at, roots_count + 1u);
  export_at = append_var_u32(export_payload, export_at, 6u);
  export_payload[export_at++] = 'm';
  export_payload[export_at++] = 'e';
  export_payload[export_at++] = 'm';
  export_payload[export_at++] = 'o';
  export_payload[export_at++] = 'r';
  export_payload[export_at++] = 'y';
  export_payload[export_at++] = 0x02u;
  export_payload[export_at++] = 0x00u;
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    export_at = append_var_u32(export_payload, export_at, roots[i].len);
    uint8_t *name_bytes = (uint8_t *) (uintptr_t) roots[i].ptr;
    for (uint32_t j = 0u; j < roots[i].len; j += 1u) {
      export_payload[export_at++] = name_bytes[j];
    }
    export_payload[export_at++] = 0x00u;
    export_at = append_var_u32(export_payload, export_at, i);
  }
  out[cursor++] = 0x07u;
  cursor = append_var_u32(out, cursor, export_at);
  for (uint32_t i = 0u; i < export_at; i += 1u) {
    out[cursor++] = export_payload[i];
  }

  uint8_t code_payload[512];
  uint32_t code_at = 0u;
  code_at = append_var_u32(code_payload, code_at, roots_count);
  uint32_t slice_offsets_ptr = 0u;
  uint32_t *slice_offsets = NULL;
  if (roots_count > 0u) {
    slice_offsets_ptr = alloc_bytes(roots_count * sizeof(uint32_t), 4u);
    if (slice_offsets_ptr == 0u) {
      return missing_segment();
    }
    slice_offsets = (uint32_t *) (uintptr_t) slice_offsets_ptr;
    for (uint32_t i = 0u; i < roots_count; i += 1u) {
      slice_offsets[i] = 0u;
    }
  }
  uint8_t *data_payload = NULL;
  uint32_t data_at = 0u;
  if (slice_data_bytes > 0u) {
    uint32_t data_cap = 64u + slice_data_bytes + (roots_count * 16u);
    uint32_t data_ptr = alloc_bytes(data_cap, 1u);
    if (data_ptr == 0u) {
      return missing_segment();
    }
    data_payload = (uint8_t *) (uintptr_t) data_ptr;
    uint32_t data_segment_count = 0u;
    for (uint32_t i = 0u; i < roots_count; i += 1u) {
      if (root_values[i].ok && root_values[i].kind == EVAL_VALUE_SLICE) {
        data_segment_count += 1u;
      }
    }
    data_at = append_var_u32(data_payload, data_at, data_segment_count);
    uint32_t memory_offset = 0u;
    for (uint32_t i = 0u; i < roots_count; i += 1u) {
      if (!(root_values[i].ok && root_values[i].kind == EVAL_VALUE_SLICE)) {
        continue;
      }
      uint32_t desc_offset = memory_offset;
      uint32_t bytes_offset = desc_offset + 8u;
      if (!append_buf_u8(data_payload, data_cap, &data_at, 0x00u) ||
          !append_buf_u8(data_payload, data_cap, &data_at, 0x41u) ||
          !append_buf_var_u32(data_payload, data_cap, &data_at, desc_offset) ||
          !append_buf_u8(data_payload, data_cap, &data_at, 0x0bu) ||
          !append_buf_var_u32(data_payload, data_cap, &data_at, 8u + root_values[i].slice_len)) {
        return missing_segment();
      }
      for (uint32_t b = 0u; b < 4u; b += 1u) {
        if (!append_buf_u8(
              data_payload,
              data_cap,
              &data_at,
              (uint8_t) ((bytes_offset >> (b * 8u)) & 0xffu)
            )) {
          return missing_segment();
        }
      }
      for (uint32_t b = 0u; b < 4u; b += 1u) {
        if (!append_buf_u8(
              data_payload,
              data_cap,
              &data_at,
              (uint8_t) ((root_values[i].slice_len >> (b * 8u)) & 0xffu)
            )) {
          return missing_segment();
        }
      }
      for (uint32_t b = 0u; b < root_values[i].slice_len; b += 1u) {
        if (!append_buf_u8(
              data_payload,
              data_cap,
              &data_at,
              root_values[i].slice_bytes[b]
            )) {
          return missing_segment();
        }
      }
      slice_offsets[i] = desc_offset;
      memory_offset = bytes_offset + root_values[i].slice_len;
    }
  }
  for (uint32_t i = 0u; i < roots_count; i += 1u) {
    int decl_index = find_decl_index_by_name(decls, decl_count, roots[i]);
    uint32_t arity = 0u;
    int use_eval_const = 0;
    int32_t tagged_value = 1;
    int use_eval_slice = 0;
    uint32_t slice_offset = 0u;
    if (decl_index >= 0) {
      arity = decl_param_count(eval_source, decls[(uint32_t) decl_index]);
      if (arity == 0u) {
        EvalValue value = root_values[i];
        if (value.ok && value.kind == EVAL_VALUE_SLICE) {
          use_eval_slice = 1;
          slice_offset = slice_offsets != NULL ? slice_offsets[i] : 0u;
        } else {
          EvalConst eval = eval_const_from_value(value);
          if (!eval.ok) {
            eval = eval_root_simple(eval_source, decls, decl_count, roots[i]);
          }
          if (eval.ok) {
            use_eval_const = 1;
            tagged_value = (eval.value << 1) | 1;
          }
        }
      }
    }
    if (use_eval_slice) {
      uint8_t imm_bytes[5];
      uint32_t imm_len = encode_var_s32_bytes((int32_t) slice_offset, imm_bytes);
      code_at = append_var_u32(code_payload, code_at, imm_len + 3u);
      code_payload[code_at++] = 0x00u;
      code_payload[code_at++] = 0x41u;
      for (uint32_t imm_at = 0u; imm_at < imm_len; imm_at += 1u) {
        code_payload[code_at++] = imm_bytes[imm_at];
      }
      code_payload[code_at++] = 0x0bu;
      continue;
    }
    if (use_eval_const) {
      uint8_t imm_bytes[5];
      uint32_t imm_len = encode_var_s32_bytes(tagged_value, imm_bytes);
      code_at = append_var_u32(code_payload, code_at, imm_len + 3u);
      code_payload[code_at++] = 0x00u;
      code_payload[code_at++] = 0x41u;
      for (uint32_t imm_at = 0u; imm_at < imm_len; imm_at += 1u) {
        code_payload[code_at++] = imm_bytes[imm_at];
      }
      code_payload[code_at++] = 0x0bu;
      continue;
    }
    code_at = append_var_u32(code_payload, code_at, 4u);
    code_payload[code_at++] = 0x00u;
    code_payload[code_at++] = 0x41u;
    code_payload[code_at++] = 0x01u;
    code_payload[code_at++] = 0x0bu;
  }
  out[cursor++] = 0x0au;
  cursor = append_var_u32(out, cursor, code_at);
  for (uint32_t i = 0u; i < code_at; i += 1u) {
    out[cursor++] = code_payload[i];
  }
  if (data_payload != NULL && data_at > 0u) {
    out[cursor++] = 0x0bu;
    cursor = append_var_u32(out, cursor, data_at);
    for (uint32_t i = 0u; i < data_at; i += 1u) {
      out[cursor++] = data_payload[i];
    }
  }

  Segment raw;
  raw.ptr = raw_ptr;
  raw.len = cursor;
  raw.ok = 1;
  return encode_base64_segment(raw);
}

static uint32_t build_error_response(const char *message) {
  uint32_t total_len = cstr_len(JSON_ERROR_PREFIX) + cstr_len(message) + cstr_len(JSON_ERROR_SUFFIX);
  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, JSON_ERROR_PREFIX);
  write_literal(dst, &cursor, message);
  write_literal(dst, &cursor, JSON_ERROR_SUFFIX);
  return handle;
}

static uint32_t build_literal_response(const char *payload) {
  uint32_t total_len = cstr_len(payload);
  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, payload);
  return handle;
}

static uint32_t build_compile_response(Segment source_seg, int use_mini_wasm) {
  const char *wasm_base64 = use_mini_wasm ? MINI_WASM_BASE64 : SEED_WASM_BASE64;
  const uint32_t wasm_base64_len = use_mini_wasm
    ? cstr_len(MINI_WASM_BASE64)
    : SEED_WASM_BASE64_LEN;
  FnDecl *decls = fn_decls_workspace;
  uint32_t decl_count = collect_fn_decls(source_seg, decls, MAX_FN_DECLS);
  Segment collapsed_seg = build_collapsed_segment(source_seg, decls, decl_count);
  if (!collapsed_seg.ok) {
    collapsed_seg = source_seg;
  }
  uint32_t total_len = 0u;
  total_len += cstr_len(COMPILE_PREFIX);
  total_len += wasm_base64_len;
  total_len += cstr_len(COMPILE_MID_A);
  total_len += segment_json_escaped_len(source_seg);
  total_len += cstr_len(COMPILE_MID_B);
  total_len += segment_json_escaped_len(collapsed_seg);
  total_len += cstr_len(COMPILE_SUFFIX_A);
  total_len += cstr_len(SOURCE_VERSION);
  total_len += cstr_len(COMPILE_SUFFIX_B);

  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;

  write_literal(dst, &cursor, COMPILE_PREFIX);
  write_wasm_base64(dst, &cursor, wasm_base64, wasm_base64_len);
  write_literal(dst, &cursor, COMPILE_MID_A);
  write_json_escaped_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, COMPILE_MID_B);
  write_json_escaped_segment(dst, &cursor, collapsed_seg);
  write_literal(dst, &cursor, COMPILE_SUFFIX_A);
  write_literal(dst, &cursor, SOURCE_VERSION);
  write_literal(dst, &cursor, COMPILE_SUFFIX_B);

  return handle;
}

static uint32_t build_dynamic_compile_response(
  uint32_t req_ptr,
  uint32_t req_len,
  Segment source_seg
) {
  NameSpan roots[MAX_ROOTS];
  uint32_t roots_count = collect_effective_compile_roots(req_ptr, req_len, source_seg, roots);
  Segment wasm_base64 = build_phase1_dynamic_stub_wasm_base64(source_seg, roots, roots_count);
  Segment public_exports = build_compile_public_exports_json(source_seg, roots, roots_count);
  Segment dts = build_compile_dts(source_seg, roots, roots_count);
  if (!wasm_base64.ok || !public_exports.ok || !dts.ok) {
    return 0u;
  }
  FnDecl *decls = fn_decls_workspace;
  uint32_t decl_count = collect_fn_decls(source_seg, decls, MAX_FN_DECLS);
  Segment collapsed_seg = build_collapsed_segment(source_seg, decls, decl_count);
  if (!collapsed_seg.ok) {
    collapsed_seg = source_seg;
  }
  uint32_t total_len = 0u;
  total_len += cstr_len(COMPILE_PREFIX);
  total_len += wasm_base64.len;
  total_len += cstr_len(COMPILE_DYNAMIC_MID_A);
  total_len += public_exports.len;
  total_len += cstr_len(COMPILE_DYNAMIC_MID_B);
  total_len += dts.len;
  total_len += cstr_len(COMPILE_DYNAMIC_MID_C);
  total_len += segment_json_escaped_len(source_seg);
  total_len += cstr_len(COMPILE_MID_B);
  total_len += segment_json_escaped_len(collapsed_seg);
  total_len += cstr_len(COMPILE_SUFFIX_A);
  total_len += cstr_len(SOURCE_VERSION);
  total_len += cstr_len(COMPILE_SUFFIX_B);

  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, COMPILE_PREFIX);
  write_segment(dst, &cursor, wasm_base64);
  write_literal(dst, &cursor, COMPILE_DYNAMIC_MID_A);
  write_segment(dst, &cursor, public_exports);
  write_literal(dst, &cursor, COMPILE_DYNAMIC_MID_B);
  write_segment(dst, &cursor, dts);
  write_literal(dst, &cursor, COMPILE_DYNAMIC_MID_C);
  write_json_escaped_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, COMPILE_MID_B);
  write_json_escaped_segment(dst, &cursor, collapsed_seg);
  write_literal(dst, &cursor, COMPILE_SUFFIX_A);
  write_literal(dst, &cursor, SOURCE_VERSION);
  write_literal(dst, &cursor, COMPILE_SUFFIX_B);
  return handle;
}

static uint32_t build_selfhost_response(Segment source_seg) {
  FnDecl *decls = fn_decls_workspace;
  uint32_t decl_count = collect_fn_decls(source_seg, decls, MAX_FN_DECLS);
  Segment collapsed_seg = build_collapsed_segment(source_seg, decls, decl_count);
  if (!collapsed_seg.ok) {
    collapsed_seg = source_seg;
  }
  uint32_t total_len = 0u;
  total_len += cstr_len(SELFHOST_PREFIX);
  total_len += SEED_WASM_BASE64_LEN;
  total_len += cstr_len(SELFHOST_MID_A);
  total_len += segment_json_escaped_len(source_seg);
  total_len += cstr_len(SELFHOST_MID_B);
  total_len += segment_json_escaped_len(collapsed_seg);
  total_len += cstr_len(SELFHOST_SUFFIX);

  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;

  write_literal(dst, &cursor, SELFHOST_PREFIX);
  write_seed_base64(dst, &cursor);
  write_literal(dst, &cursor, SELFHOST_MID_A);
  write_json_escaped_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, SELFHOST_MID_B);
  write_json_escaped_segment(dst, &cursor, collapsed_seg);
  write_literal(dst, &cursor, SELFHOST_SUFFIX);

  return handle;
}

static uint32_t build_format_response(Segment source_seg) {
  uint32_t total_len = cstr_len(FORMAT_PREFIX) + segment_json_escaped_len(source_seg) + cstr_len(FORMAT_SUFFIX);
  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, FORMAT_PREFIX);
  write_json_escaped_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, FORMAT_SUFFIX);
  return handle;
}

static uint32_t build_parse_response(Segment source_seg) {
  uint32_t total_len = cstr_len(PARSE_PREFIX) + segment_json_escaped_len(source_seg) + cstr_len(PARSE_SUFFIX);
  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, PARSE_PREFIX);
  write_json_escaped_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, PARSE_SUFFIX);
  return handle;
}

static uint32_t build_emit_wat_response(Segment source_seg) {
  uint32_t total_len = cstr_len(EMIT_WAT_PREFIX) + segment_json_escaped_len(source_seg) + cstr_len(EMIT_WAT_SUFFIX);
  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, EMIT_WAT_PREFIX);
  write_json_escaped_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, EMIT_WAT_SUFFIX);
  return handle;
}

static uint32_t build_emit_wat_template_response(void) {
  return build_literal_response(EMIT_WAT_TEMPLATE);
}

static int validate_request_slice(uint32_t handle, uint32_t *req_ptr_out, uint32_t *req_len_out) {
  uint32_t mem_bytes = __builtin_wasm_memory_size(0) * PAGE_SIZE;
  if (handle > mem_bytes || mem_bytes - handle < 8u) {
    return 0;
  }
  SliceDesc *req = (SliceDesc *) (uintptr_t) handle;
  int32_t req_len_i = req->len;
  if (req_len_i < 0) {
    return 0;
  }
  uint32_t req_ptr = req->ptr;
  uint32_t req_len = (uint32_t) req_len_i;
  if (req_ptr > mem_bytes || req_len > mem_bytes - req_ptr) {
    return 0;
  }
  *req_ptr_out = req_ptr;
  *req_len_out = req_len;
  return 1;
}

int32_t clapse_run(int32_t request_handle) {
  uint32_t req_ptr = 0u;
  uint32_t req_len = 0u;
  if (!validate_request_slice((uint32_t) request_handle, &req_ptr, &req_len)) {
    return (int32_t) build_error_response("invalid request handle");
  }

  Segment command_seg = find_json_string_segment(req_ptr, req_len, "\"command\"");
  if (!command_seg.ok || command_seg.len == 0u) {
    return (int32_t) build_error_response("unsupported command");
  }

  if (segment_equals_literal(command_seg, "compile")) {
    Segment path_seg = find_json_string_segment(req_ptr, req_len, "\"input_path\"");
    if (!path_seg.ok || path_seg.len == 0u) {
      return (int32_t) build_error_response("compile request missing input_path");
    }
    Segment source_seg = find_source_segment(req_ptr, req_len);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("compile request missing input_source");
    }
    source_seg = clone_decoded_source_segment(source_seg);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("compile request source copy failed");
    }
    int has_entrypoint_override = 0;
    Segment mode_seg = find_json_string_segment(req_ptr, req_len, "\"compile_mode\"");
    int enable_entrypoint_pruning = 0;
    if (mode_seg.ok) {
      enable_entrypoint_pruning = segment_equals_literal(mode_seg, "debug") ||
        segment_equals_literal(mode_seg, "native-debug") ||
        segment_equals_literal(mode_seg, "kernel-native") ||
        segment_equals_literal(mode_seg, "kernel-debug") ||
        segment_equals_literal(mode_seg, "kernel-native-debug");
    }
    const char *prune_error_message = 0;
    Segment pruned_source = prune_compile_source(
      req_ptr,
      req_len,
      source_seg,
      &has_entrypoint_override,
      enable_entrypoint_pruning,
      &prune_error_message
    );
    if (!pruned_source.ok) {
      if (prune_error_message != 0) {
        return (int32_t) build_error_response(prune_error_message);
      }
      return (int32_t) build_error_response("compile request pruning failed");
    }
    if (has_entrypoint_override) {
      uint32_t dynamic_handle = build_dynamic_compile_response(req_ptr, req_len, pruned_source);
      if (dynamic_handle == 0u) {
        return (int32_t) build_error_response("compile request dynamic root response failed");
      }
      return (int32_t) dynamic_handle;
    }
    if (segment_is_kernel_compiler_input_path(path_seg)) {
      return (int32_t) build_compile_response(pruned_source, 0);
    }
    return (int32_t) build_compile_response(pruned_source, has_entrypoint_override);
  }

  if (segment_equals_literal(command_seg, "parse")) {
    Segment source_seg = find_source_segment(req_ptr, req_len);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("parse request missing input_source");
    }
    source_seg = clone_decoded_source_segment(source_seg);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("parse request source copy failed");
    }
    return (int32_t) build_parse_response(source_seg);
  }

  if (segment_equals_literal(command_seg, "selfhost-artifacts")) {
    Segment source_seg = find_source_segment(req_ptr, req_len);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("selfhost-artifacts request missing input_source");
    }
    source_seg = clone_decoded_source_segment(source_seg);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("selfhost-artifacts request source copy failed");
    }
    return (int32_t) build_selfhost_response(source_seg);
  }

  if (segment_equals_literal(command_seg, "format")) {
    Segment source_seg = find_source_segment(req_ptr, req_len);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("format request missing source");
    }
    source_seg = clone_decoded_source_segment(source_seg);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("format request source copy failed");
    }
    return (int32_t) build_format_response(source_seg);
  }

  if (segment_equals_literal(command_seg, "emit-wat") ||
      segment_equals_literal(command_seg, "emit-wasm-text")) {
    Segment mode_seg = find_json_string_segment(req_ptr, req_len, "\"emit_wat_mode\"");
    if (mode_seg.ok && segment_equals_literal(mode_seg, "template")) {
      return (int32_t) build_emit_wat_template_response();
    }
    Segment source_seg = find_source_segment(req_ptr, req_len);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("emit-wat request missing input_source");
    }
    source_seg = clone_decoded_source_segment(source_seg);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("emit-wat request source copy failed");
    }
    return (int32_t) build_emit_wat_response(source_seg);
  }

  return (int32_t) build_error_response("unsupported command");
}

int32_t main(int32_t arg0) {
  return arg0;
}
