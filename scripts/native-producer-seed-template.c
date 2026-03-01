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

static const char SOURCE_VERSION[] = "{{SOURCE_VERSION_LITERAL}}";

static const char JSON_ERROR_PREFIX[] = "{\"ok\":false,\"error\":\"";
static const char JSON_ERROR_SUFFIX[] = "\"}";

static const char COMPILE_PREFIX[] = "{\"ok\":true,\"backend\":\"kernel-native\",\"wasm_base64\":\"";
static const char COMPILE_MID_A[] = "\",\"exports\":[{\"name\":\"clapse_run\",\"arity\":1},{\"name\":\"main\",\"arity\":1}],\"dts\":\"export declare function clapse_run(request_handle: number): number;\\nexport declare function main(arg0: number): number;\\n\",\"artifacts\":{\"lowered_ir.txt\":\"(lowered_ir) ";
static const char COMPILE_MID_B[] = "\",\"collapsed_ir.txt\":\"(collapsed_ir) ";
static const char COMPILE_SUFFIX_A[] = "\"},\"__clapse_contract\":{\"source_version\":\"";
static const char COMPILE_SUFFIX_B[] = "\",\"compile_contract_version\":\"native-v1\"}}";

static const char SELFHOST_PREFIX[] = "{\"ok\":true,\"backend\":\"kernel-native\",\"wasm_base64\":\"";
static const char SELFHOST_MID_A[] = "\",\"artifacts\":{\"lowered_ir.txt\":\"(lowered_ir) ";
static const char SELFHOST_MID_B[] = "\",\"collapsed_ir.txt\":\"(collapsed_ir) ";
static const char SELFHOST_SUFFIX[] = "\"}}";

static const char FORMAT_PREFIX[] = "{\"ok\":true,\"formatted\":\"";
static const char FORMAT_SUFFIX[] = "\"}";

static const char EMIT_WAT_PREFIX[] = "{\"ok\":true,\"wat\":\"";
static const char EMIT_WAT_SUFFIX[] = "\"}";
static const char EMIT_WAT_TEMPLATE[] =
  "{\"ok\":true,\"wat\":\"(module\\n  (memory (export \\\"__memory\\\") 1)\\n)\"}";

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

static void write_seed_base64(uint8_t *dst, uint32_t *cursor) {
  uint32_t len = cstr_len(SEED_WASM_BASE64);
  for (uint32_t i = 0; i < len; i += 1) {
    dst[*cursor + i] = (uint8_t) SEED_WASM_BASE64[i];
  }
  *cursor += len;
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

static uint32_t build_compile_response(Segment source_seg) {
  uint32_t total_len = 0u;
  total_len += cstr_len(COMPILE_PREFIX);
  total_len += cstr_len(SEED_WASM_BASE64);
  total_len += cstr_len(COMPILE_MID_A);
  total_len += source_seg.len;
  total_len += cstr_len(COMPILE_MID_B);
  total_len += source_seg.len;
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
  write_seed_base64(dst, &cursor);
  write_literal(dst, &cursor, COMPILE_MID_A);
  write_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, COMPILE_MID_B);
  write_segment(dst, &cursor, source_seg);
  write_literal(dst, &cursor, COMPILE_SUFFIX_A);
  write_literal(dst, &cursor, SOURCE_VERSION);
  write_literal(dst, &cursor, COMPILE_SUFFIX_B);

  return handle;
}

static uint32_t build_selfhost_response(Segment source_seg) {
  uint32_t total_len = 0u;
  total_len += cstr_len(SELFHOST_PREFIX);
  total_len += cstr_len(SEED_WASM_BASE64);
  total_len += cstr_len(SELFHOST_MID_A);
  total_len += source_seg.len;
  total_len += cstr_len(SELFHOST_MID_B);
  total_len += source_seg.len;
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
  write_segment(dst, &cursor, source_seg);
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
    return (int32_t) build_compile_response(source_seg);
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
