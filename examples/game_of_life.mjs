import { decodeInt, encodeInt, instantiateWithRuntime } from "../scripts/wasm-runtime.mjs";

const GRID_W = 160;
const GRID_H = 100;

const canvas = document.getElementById("life_canvas");
const ctx = canvas.getContext("2d");
const toggleBtn = document.getElementById("toggle");
const stepBtn = document.getElementById("step");
const randomizeBtn = document.getElementById("randomize");
const clearBtn = document.getElementById("clear");
const speedSlider = document.getElementById("speed");
const speedValue = document.getElementById("speed_value");
const stats = document.getElementById("stats");

canvas.width = GRID_W;
canvas.height = GRID_H;

const image = ctx.createImageData(GRID_W, GRID_H);
const pixels = image.data;
const taggedGridW = encodeInt(GRID_W);
const taggedGridH = encodeInt(GRID_H);
const MAX_CATCHUP_STEPS = 2;
const STATS_UPDATE_MS = 250;

let generation = 0;
let running = true;
let tps = Number(speedSlider.value);
let accumulatorMs = 0;
let lastTimeMs = performance.now();
let nextStatsDueMs = lastTimeMs + STATS_UPDATE_MS;
let aliveCells = 0;
let boardDirty = true;
let statsDirty = true;
let boardView = new Uint8Array(GRID_W * GRID_H);
let lifeFns = null;
let lifeState = null;
let runtimeBridge = null;

function syncFromState() {
  if (!lifeFns || !runtimeBridge || lifeState === null) {
    return;
  }
  const currentHandle = lifeFns.stateCurrent(lifeState);
  boardView = runtimeBridge.read_slice_u8(currentHandle);
  generation = decodeInt(lifeFns.stateGeneration(lifeState));
  aliveCells = decodeInt(lifeFns.stateAliveCount(taggedGridW, taggedGridH, lifeState));
  boardDirty = true;
  statsDirty = true;
}

function dispatchLifeEvent(eventHandle) {
  if (!lifeFns || lifeState === null) {
    return;
  }
  lifeState = lifeFns.applyEvent(taggedGridW, taggedGridH, eventHandle, lifeState);
  syncFromState();
}

function initializeLifeState() {
  if (!lifeFns || !runtimeBridge) {
    return;
  }
  const empty = new Uint8Array(GRID_W * GRID_H);
  const currentHandle = runtimeBridge.alloc_slice_u8(empty);
  const nextHandle = runtimeBridge.alloc_slice_u8(empty);
  lifeState = lifeFns.initState(currentHandle, nextHandle);
  syncFromState();
}

function clearBoard() {
  if (!lifeFns || lifeState === null) {
    return;
  }
  dispatchLifeEvent(lifeFns.eventClear(encodeInt(0)));
  render(performance.now());
}

function randomizeBoard() {
  if (!lifeFns || !runtimeBridge || lifeState === null) {
    return;
  }
  const seeded = new Uint8Array(GRID_W * GRID_H);
  for (let i = 0; i < seeded.length; i += 1) {
    seeded[i] = Math.random() < 0.28 ? 1 : 0;
  }
  const seededHandle = runtimeBridge.alloc_slice_u8(seeded);
  dispatchLifeEvent(lifeFns.eventLoad(seededHandle));
  render(performance.now());
}

function updateStats() {
  stats.textContent = `gen=${generation} alive=${aliveCells}`;
}

function render(nowMs = performance.now()) {
  if (boardDirty) {
    for (let i = 0; i < boardView.length; i += 1) {
      const pixelIx = i * 4;
      if (boardView[i] === 1) {
        pixels[pixelIx] = 34;
        pixels[pixelIx + 1] = 197;
        pixels[pixelIx + 2] = 94;
        pixels[pixelIx + 3] = 255;
      } else {
        pixels[pixelIx] = 11;
        pixels[pixelIx + 1] = 18;
        pixels[pixelIx + 2] = 32;
        pixels[pixelIx + 3] = 255;
      }
    }
    ctx.putImageData(image, 0, 0);
    boardDirty = false;
  }

  if (statsDirty || nowMs >= nextStatsDueMs) {
    updateStats();
    statsDirty = false;
    nextStatsDueMs = nowMs + STATS_UPDATE_MS;
  }
}

function stepSimulation(stepCount = 1) {
  if (!lifeFns || lifeState === null || stepCount <= 0) {
    return;
  }
  dispatchLifeEvent(lifeFns.eventTick(encodeInt(stepCount)));
}

function frame(nowMs) {
  const deltaMs = nowMs - lastTimeMs;
  lastTimeMs = nowMs;
  const stepMs = 1000 / tps;

  if (running) {
    accumulatorMs = Math.min(accumulatorMs + deltaMs, stepMs * MAX_CATCHUP_STEPS);
    const stepCount = Math.floor(accumulatorMs / stepMs);
    if (stepCount > 0) {
      stepSimulation(stepCount);
      accumulatorMs -= stepCount * stepMs;
    }
  }

  render(nowMs);
  requestAnimationFrame(frame);
}

function setRunning(nextRunning) {
  running = nextRunning;
  toggleBtn.textContent = running ? "Pause" : "Play";
}

function updateSpeed() {
  tps = Number(speedSlider.value);
  speedValue.textContent = `${tps} tps`;
  const stepMs = 1000 / tps;
  accumulatorMs = Math.min(accumulatorMs, stepMs * MAX_CATCHUP_STEPS);
  statsDirty = true;
}

function toggleCellFromEvent(event) {
  if (!lifeFns || lifeState === null) {
    return;
  }
  const rect = canvas.getBoundingClientRect();
  const x = Math.floor(((event.clientX - rect.left) / rect.width) * GRID_W);
  const y = Math.floor(((event.clientY - rect.top) / rect.height) * GRID_H);
  if (x < 0 || x >= GRID_W || y < 0 || y >= GRID_H) {
    return;
  }
  dispatchLifeEvent(lifeFns.eventToggle(encodeInt(x), encodeInt(y)));
  render(performance.now());
}

toggleBtn.addEventListener("click", () => {
  setRunning(!running);
});

stepBtn.addEventListener("click", () => {
  stepSimulation();
  render(performance.now());
});

randomizeBtn.addEventListener("click", () => {
  randomizeBoard();
});

clearBtn.addEventListener("click", () => {
  clearBoard();
});

speedSlider.addEventListener("input", () => {
  updateSpeed();
});

canvas.addEventListener("click", toggleCellFromEvent);

async function loadWasmLifeRule() {
  const response = await fetch("../out/game_of_life.wasm");
  if (!response.ok) {
    throw new Error(`failed to load ../out/game_of_life.wasm: ${response.status}`);
  }
  const bytes = await response.arrayBuffer();
  const { instance, runtime } = await instantiateWithRuntime(bytes);
  const initState = instance.exports.init_state;
  const applyEvent = instance.exports.apply_event;
  const eventTick = instance.exports.event_tick;
  const eventToggle = instance.exports.event_toggle;
  const eventClear = instance.exports.event_clear;
  const eventLoad = instance.exports.event_load;
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
    || typeof stateCurrent !== "function"
    || typeof stateGeneration !== "function"
    || typeof stateAliveCount !== "function"
  ) {
    throw new Error(
      "expected exported wasm functions: init_state, apply_event, event_tick, event_toggle, event_clear, event_load, state_current, state_generation, state_alive_count",
    );
  }
  return {
    runtime,
    lifeFns: {
      initState,
      applyEvent,
      eventTick,
      eventToggle,
      eventClear,
      eventLoad,
      stateCurrent,
      stateGeneration,
      stateAliveCount,
    },
  };
}

async function boot() {
  try {
    const loaded = await loadWasmLifeRule();
    runtimeBridge = loaded.runtime;
    lifeFns = loaded.lifeFns;
    initializeLifeState();
    randomizeBoard();
    updateSpeed();
    lastTimeMs = performance.now();
    nextStatsDueMs = lastTimeMs + STATS_UPDATE_MS;
    requestAnimationFrame(frame);
  } catch (err) {
    setRunning(false);
    const message = err instanceof Error ? err.message : String(err);
    stats.textContent = `error: ${message}`;
    console.error(err);
  }
}

boot();
