#!/usr/bin/env node

import { cliArgs, failWithError, readBinaryFile } from "./runtime-env.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

function indexOf(w, x, y) {
  return y * w + x;
}

function expectEq(label, expected, actual) {
  if (expected !== actual) {
    throw new Error(`${label}: expected ${expected}, got ${actual}`);
  }
}

async function main() {
  const [wasmPathArg] = cliArgs();
  const wasmPath = wasmPathArg ?? "out/game_of_life.wasm";
  const wasmBytes = await readBinaryFile(wasmPath);
  const { instance, runtime } = await instantiateWithRuntime(wasmBytes);
  const initState = instance.exports.init_state;
  const applyEvent = instance.exports.apply_event;
  const eventTick = instance.exports.event_tick;
  const eventToggle = instance.exports.event_toggle;
  const eventClear = instance.exports.event_clear;
  const eventLoad = instance.exports.event_load;
  const stepState = instance.exports.step_state;
  const stepStateN = instance.exports.step_state_n;
  const stateCurrent = instance.exports.state_current;
  const stateGeneration = instance.exports.state_generation;
  const stateAliveCount = instance.exports.state_alive_count;
  if (
    typeof initState !== "function"
    || typeof applyEvent !== "function"
    || typeof eventTick !== "function"
    || typeof eventToggle !== "function"
    || typeof eventClear !== "function"
    || typeof eventLoad !== "function"
    || typeof stepState !== "function"
    || typeof stepStateN !== "function"
    || typeof stateCurrent !== "function"
    || typeof stateGeneration !== "function"
    || typeof stateAliveCount !== "function"
  ) {
    throw new Error(
      "missing exports: init_state/apply_event/event_tick/event_toggle/event_clear/event_load/step_state/step_state_n/state_current/state_generation/state_alive_count",
    );
  }

  const width = 5;
  const height = 5;
  const taggedW = encodeInt(width);
  const taggedH = encodeInt(height);

  const board = new Uint8Array(width * height);
  board[indexOf(width, 1, 2)] = 1;
  board[indexOf(width, 2, 2)] = 1;
  board[indexOf(width, 3, 2)] = 1;

  const inHandle = runtime.alloc_slice_u8(board);
  const outHandle0 = runtime.alloc_slice_u8(new Uint8Array(width * height));
  let state = initState(inHandle, outHandle0);
  state = stepState(taggedW, taggedH, state);
  const outHandle1 = stateCurrent(state);
  const nextBoard = new Uint8Array(width * height);
  runtime.copy_slice_u8(outHandle1, nextBoard);
  expectEq("generation", 1, decodeInt(stateGeneration(state)));
  expectEq("alive count", 3, decodeInt(stateAliveCount(taggedW, taggedH, state)));

  expectEq("cell (2,1)", 1, nextBoard[indexOf(width, 2, 1)]);
  expectEq("cell (2,2)", 1, nextBoard[indexOf(width, 2, 2)]);
  expectEq("cell (2,3)", 1, nextBoard[indexOf(width, 2, 3)]);
  expectEq("cell (1,2)", 0, nextBoard[indexOf(width, 1, 2)]);
  expectEq("cell (3,2)", 0, nextBoard[indexOf(width, 3, 2)]);

  state = stepStateN(taggedW, taggedH, encodeInt(2), state);
  expectEq("generation after +2", 3, decodeInt(stateGeneration(state)));

  const eventInHandle = runtime.alloc_slice_u8(board);
  const eventOutHandle = runtime.alloc_slice_u8(new Uint8Array(width * height));
  let eventState = initState(eventInHandle, eventOutHandle);
  eventState = applyEvent(taggedW, taggedH, eventTick(encodeInt(1)), eventState);
  expectEq("event generation", 1, decodeInt(stateGeneration(eventState)));
  expectEq("event alive count", 3, decodeInt(stateAliveCount(taggedW, taggedH, eventState)));
  eventState = applyEvent(taggedW, taggedH, eventClear(encodeInt(0)), eventState);
  expectEq("event clear alive count", 0, decodeInt(stateAliveCount(taggedW, taggedH, eventState)));
  const loadSeed = new Uint8Array(width * height);
  loadSeed[indexOf(width, 0, 0)] = 1;
  eventState = applyEvent(taggedW, taggedH, eventLoad(runtime.alloc_slice_u8(loadSeed)), eventState);
  expectEq("event load generation reset", 0, decodeInt(stateGeneration(eventState)));
  eventState = applyEvent(taggedW, taggedH, eventToggle(encodeInt(0), encodeInt(0)), eventState);
  expectEq("event toggle off", 0, decodeInt(stateAliveCount(taggedW, taggedH, eventState)));
  eventState = applyEvent(taggedW, taggedH, eventToggle(encodeInt(1), encodeInt(1)), eventState);
  expectEq("event toggle on", 1, decodeInt(stateAliveCount(taggedW, taggedH, eventState)));

  console.log("life slice smoke: PASS");
}

main().catch(failWithError);
