import { decodeInt, encodeInt, instantiateWithRuntime, isTaggedInt } from "./scripts/wasm-runtime.mjs";
import fs from "node:fs";

function randomBoard(width, height) {
  const board = new Uint8Array(width * height);
  for (let i = 0; i < board.length; i += 1) {
    board[i] = Math.random() < 0.25 ? 1 : 0;
  }
  return board;
}

async function main() {
  const wasmPath = "out/game_of_life.wasm";
  const wasmBytes = fs.readFileSync(wasmPath);
  const { instance, runtime } = await instantiateWithRuntime(wasmBytes);

  const initState = instance.exports.init_state;
  const stepStateN = instance.exports.step_state_n;

  if (typeof initState !== "function" || typeof stepStateN !== "function") {
    throw new Error("missing expected exports");
  }

  const width = Number(process.argv[2] ?? "160");
  const height = Number(process.argv[3] ?? "100");
  const taggedW = encodeInt(width);
  const taggedH = encodeInt(height);

  const iterations = Number(process.argv[4] ?? "500");
  const jump = Number(process.argv[5] ?? "1");

  const board = randomBoard(width, height);
  let state = initState(runtime.alloc_slice_u8(board), runtime.alloc_slice_u8(new Uint8Array(board.length)));

  for (let i = 0; i < 10; i += 1) {
    state = stepStateN(taggedW, taggedH, encodeInt(1), state);
  }

  const allocBefore = runtime.state.nextAlloc ?? 0;
  const memoryBefore = runtime.state.memory?.buffer.byteLength ?? 0;

  const stepArg = encodeInt(jump);
  const start = process.hrtime.bigint();
  for (let i = 0; i < iterations; i += 1) {
    state = stepStateN(taggedW, taggedH, stepArg, state);
  }
  const end = process.hrtime.bigint();
  const ns = Number(end - start);
  const allocAfter = runtime.state.nextAlloc ?? 0;
  const memoryAfter = runtime.state.memory?.buffer.byteLength ?? 0;

  const ms = ns / 1_000_000;
  const msPerCall = ms / iterations;
  const cells = width * height;
  const nsPerStep = ns / iterations;
  const nsPerCell = nsPerStep / cells;
  const allocDelta = allocAfter - allocBefore;

  console.log(`width=${width}`);
  console.log(`height=${height}`);
  console.log(`cells=${cells}`);
  console.log(`iterations=${iterations}`);
  console.log(`jump=${jump}`);
  console.log(`elapsed_ms=${ms.toFixed(3)}`);
  console.log(`ms_per_step=${msPerCall.toFixed(6)}`);
  console.log(`ns_per_step=${nsPerStep.toFixed(2)}`);
  console.log(`ns_per_cell=${nsPerCell.toFixed(2)}`);
  console.log(`steps_per_sec=${(1000 / msPerCall).toFixed(2)}`);
  console.log(`alloc_before=${allocBefore}`);
  console.log(`alloc_after=${allocAfter}`);
  console.log(`alloc_delta=${allocDelta}`);
  console.log(`alloc_bytes_per_step=${(allocDelta / iterations).toFixed(2)}`);
  console.log(`memory_before=${memoryBefore}`);
  console.log(`memory_after=${memoryAfter}`);

  const generationValue = instance.exports.state_generation(state);
  if (isTaggedInt(generationValue)) {
    console.log(`generation=${decodeInt(generationValue)}`);
  } else {
    console.log(`generation_raw_non_tagged=${generationValue}`);
  }
}

main().catch((err) => {
  console.error("timing failed");
  console.error(err.stack ?? String(err));
  process.exit(1);
});
