#!/usr/bin/env node

import fs from "node:fs";
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
  const wasmPath = process.argv[2] ?? "out/game_of_life.wasm";
  const wasmBytes = fs.readFileSync(wasmPath);
  const { instance, runtime } = await instantiateWithRuntime(wasmBytes);
  const initState = instance.exports.init_state;
  const stepState = instance.exports.step_state;
  const stepStateN = instance.exports.step_state_n;
  const stateCurrent = instance.exports.state_current;
  const stateGeneration = instance.exports.state_generation;
  const stateAliveCount = instance.exports.state_alive_count;
  if (
    typeof initState !== "function"
    || typeof stepState !== "function"
    || typeof stepStateN !== "function"
    || typeof stateCurrent !== "function"
    || typeof stateGeneration !== "function"
    || typeof stateAliveCount !== "function"
  ) {
    throw new Error("missing exports: init_state/step_state/step_state_n/state_current/state_generation/state_alive_count");
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

  console.log("life slice smoke: PASS");
}

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});
