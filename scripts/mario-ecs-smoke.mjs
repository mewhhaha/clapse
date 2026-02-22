import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";
import { readFile } from "node:fs/promises";

async function run() {
  const wasm_path = process.argv[2] ?? "out/mario_ecs.wasm";
  const bytes = await readFile(wasm_path);

  const loaded = await instantiateWithRuntime(bytes);
  const e = loaded.instance.exports;

  const required = [
    "world_width",
    "world_height",
    "entity_capacity",
    "init_state",
    "apply_event",
    "event_tick",
    "event_reset",
    "state_score",
    "state_lives",
    "state_frame",
    "state_active_count",
  ];

  for (const name of required) {
    if (typeof e[name] !== "function") {
      throw new Error(`missing export: ${name}`);
    }
  }

  const world_width = decodeInt(e.world_width(encodeInt(0)));
  const entity_capacity = decodeInt(e.entity_capacity(encodeInt(0)));

  if (world_width <= 0 || entity_capacity <= 0) {
    throw new Error("invalid world/entity constants");
  }

  const kinds = loaded.runtime.alloc_slice_u8(new Uint8Array(entity_capacity));
  const xs = loaded.runtime.alloc_slice_u8(new Uint8Array(entity_capacity));
  const lanes = loaded.runtime.alloc_slice_u8(new Uint8Array(entity_capacity));
  const active = loaded.runtime.alloc_slice_u8(new Uint8Array(entity_capacity));

  let state = e.init_state(kinds, xs, lanes, active);
  state = e.apply_event(e.event_reset(encodeInt(0)), state);

  for (let i = 0; i < 40; i += 1) {
    const left = i % 8 === 0 ? 1 : 0;
    const right = i % 3 === 0 ? 1 : 0;
    const jump = i % 7 === 0 ? 1 : 0;
    const event = e.event_tick(encodeInt(left), encodeInt(right), encodeInt(jump));
    state = e.apply_event(event, state);
  }

  const frame_no = decodeInt(e.state_frame(state));
  const score = decodeInt(e.state_score(state));
  const lives = decodeInt(e.state_lives(state));
  const active_count = decodeInt(e.state_active_count(state));

  if (frame_no <= 0) {
    throw new Error(`expected frame to advance, got ${frame_no}`);
  }

  if (score < 0) {
    throw new Error(`score should be non-negative, got ${score}`);
  }

  if (lives < 0) {
    throw new Error(`lives should be non-negative, got ${lives}`);
  }

  if (active_count < 0 || active_count > entity_capacity) {
    throw new Error(`active_count out of range: ${active_count}`);
  }

  console.log(`mario smoke: PASS frame=${frame_no} score=${score} lives=${lives} active=${active_count}`);
}

run().catch((error) => {
  const message = error instanceof Error ? error.message : String(error);
  console.error(`mario smoke: FAIL ${message}`);
  process.exit(1);
});
