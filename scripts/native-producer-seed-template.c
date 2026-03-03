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

static const char COMPILE_PREFIX[] = "{\"ok\":true,\"backend\":\"kernel-native\",\"wasm_base64\":\"";
static const char COMPILE_MID_A[] = "\",\"exports\":[{\"name\":\"clapse_run\",\"arity\":1},{\"name\":\"main\",\"arity\":1}],\"dts\":\"export declare function clapse_run(request_handle: number): number;\\nexport declare function main(arg0: number): number;\\n\",\"artifacts\":{\"lowered_ir.txt\":\"(lowered_ir) ";
static const char COMPILE_MID_B[] = "\",\"collapsed_ir.txt\":\"(collapsed_ir) ";
static const char COMPILE_SUFFIX_A[] = "\"},\"__clapse_contract\":{\"source_version\":\"";
static const char COMPILE_SUFFIX_B[] = "\",\"compile_contract_version\":\"native-v1\"}}";

static const char SELFHOST_PREFIX[] = "{\"ok\":true,\"backend\":\"kernel-native\",\"wasm_base64\":\"";
static const char SELFHOST_MID_A[] = "\",\"artifacts\":{\"lowered_ir.txt\":\"(lowered_ir) ";
static const char SELFHOST_MID_B[] = "\",\"collapsed_ir.txt\":\"(collapsed_ir) ";
static const char SELFHOST_SUFFIX[] = "\"}}";
static const char TAIL_SELF_PREFIX[] = "\\n-- VSelfTailCall ";
static const char TAIL_MUTUAL_PREFIX[] = "\\n-- VMutualTailCall ";
static const char TAIL_ARROW[] = " -> ";

static const char FORMAT_PREFIX[] = "{\"ok\":true,\"formatted\":\"";
static const char FORMAT_SUFFIX[] = "\"}";

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
  NameSpan name;
  uint32_t line_start;
  uint32_t line_end;
  uint32_t body_start;
  uint32_t body_end;
} FnDecl;

static uint32_t collect_fn_decls(Segment source, FnDecl *decls, uint32_t max_decls);

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
    if (b0 == '\\' && i + 1u < source.len && mem[i + 1u] == 'n') {
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
  if (mem[line_end] == '\\' && line_end + 1u < source.len &&
      mem[line_end + 1u] == 'n') {
    return line_end + 2u;
  }
  return source.len;
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
    namespan_equals_literal(name, "primitive") ||
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
      continue;
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

static int parse_top_level_decl(Segment source, uint32_t line_start, uint32_t line_end, FnDecl *out) {
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
  uint32_t eq_at = name_end;
  while (eq_at < line_end && mem[eq_at] != '=') {
    eq_at += 1u;
  }
  if (eq_at >= line_end) {
    return 0;
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

static uint32_t collect_export_roots_from_source(Segment source, NameSpan *roots, uint32_t roots_count) {
  uint8_t *mem = (uint8_t *) (uintptr_t) source.ptr;
  uint32_t line_start = 0u;
  while (line_start < source.len) {
    uint32_t line_end = source_line_end(source, line_start);
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
    uint32_t next_line = source_next_line_start(source, line_end);
    if (count < max_decls) {
      FnDecl decl;
      if (parse_top_level_decl(source, line_start, line_end, &decl)) {
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
      uint32_t at = decls[i].body_start;
      while (at < decls[i].body_end) {
        if (mem[at] == '-' && at + 1u < decls[i].body_end && mem[at + 1u] == '-') {
          at = source_line_end(source, at);
          continue;
        }
        if (mem[at] == '"') {
          uint32_t next = json_string_end(source.ptr, at, decls[i].body_end);
          at = next > at ? next : at + 1u;
          continue;
        }
        uint32_t tok_end = at;
        if (is_ident_start(mem[at])) {
          tok_end = source_parse_ident_end(source, at, decls[i].body_end);
        } else if (is_operator_start(mem[at])) {
          tok_end = source_parse_operator_end(source, at, decls[i].body_end);
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
  total_len += source_seg.len;
  total_len += cstr_len(COMPILE_MID_B);
  total_len += collapsed_seg.len;
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
  write_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, COMPILE_MID_B);
  write_segment(dst, &cursor, collapsed_seg);
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
  total_len += source_seg.len;
  total_len += cstr_len(SELFHOST_MID_B);
  total_len += collapsed_seg.len;
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
  write_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, SELFHOST_MID_B);
  write_segment(dst, &cursor, collapsed_seg);
  write_literal(dst, &cursor, SELFHOST_SUFFIX);

  return handle;
}

static uint32_t build_format_response(Segment source_seg) {
  uint32_t total_len = cstr_len(FORMAT_PREFIX) + source_seg.len + cstr_len(FORMAT_SUFFIX);
  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, FORMAT_PREFIX);
  write_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, FORMAT_SUFFIX);
  return handle;
}

static uint32_t build_emit_wat_response(Segment source_seg) {
  uint32_t total_len = cstr_len(EMIT_WAT_PREFIX) + source_seg.len + cstr_len(EMIT_WAT_SUFFIX);
  uint32_t payload_ptr = 0u;
  uint32_t handle = make_slice_response(total_len, &payload_ptr);
  if (handle == 0u) {
    return 0u;
  }
  uint8_t *dst = (uint8_t *) (uintptr_t) payload_ptr;
  uint32_t cursor = 0u;
  write_literal(dst, &cursor, EMIT_WAT_PREFIX);
  write_segment(dst, &cursor, source_seg);
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
    source_seg = clone_segment(source_seg);
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
    return (int32_t) build_compile_response(pruned_source, has_entrypoint_override);
  }

  if (segment_equals_literal(command_seg, "selfhost-artifacts")) {
    Segment source_seg = find_source_segment(req_ptr, req_len);
    if (!source_seg.ok || source_seg.len == 0u) {
      return (int32_t) build_error_response("selfhost-artifacts request missing input_source");
    }
    source_seg = clone_segment(source_seg);
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
    source_seg = clone_segment(source_seg);
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
    source_seg = clone_segment(source_seg);
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
