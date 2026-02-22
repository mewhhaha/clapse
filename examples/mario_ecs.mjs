import { decodeInt, encodeInt, instantiateWithRuntime } from "../scripts/wasm-runtime.mjs";

const canvas = document.getElementById("mario_canvas");
const ctx = canvas.getContext("2d");
const reset_btn = document.getElementById("reset");
const stats = document.getElementById("stats");

const atlas = {
  mario_and_items: {
    mario_idle: { x: 18, y: 2, w: 18, h: 34 },
    mario_walk_a: { x: 36, y: 2, w: 18, h: 34 },
    mario_walk_b: { x: 54, y: 2, w: 18, h: 34 },
    mario_jump: { x: 90, y: 2, w: 16, h: 34 },
    coin_small: { x: 1, y: 73, w: 15, h: 17 },
  },
  enemies: {
    goomba_walk_a: { x: 0, y: 0, w: 36, h: 36 },
    goomba_walk_b: { x: 36, y: 0, w: 36, h: 36 },
  },
  blocks: {
    ground_brick_brown: { x: 0, y: 0, w: 16, h: 16 },
    question_block_brown: { x: 95, y: 0, w: 16, h: 16 },
  },
};

const key_state = {
  left: false,
  right: false,
  jump: false,
};

let images = null;
let runtime = null;
let fns = null;
let state = null;
let world_w = 48;
let world_h = 15;
let entity_capacity = 12;
let tick_accum_ms = 0;
let last_time_ms = performance.now();

const tile_px = 16;
const ticks_per_second = 16;
const max_catchup_ticks = 3;

function load_image(src) {
  return new Promise((resolve, reject) => {
    const img = new Image();
    img.onload = () => resolve(img);
    img.onerror = () => reject(new Error(`failed to load image: ${src}`));
    img.src = src;
  });
}

function draw_region(image, region, dx, dy, dw, dh, flip_x = false) {
  if (!image || !region) {
    return;
  }
  if (flip_x) {
    ctx.save();
    ctx.translate(dx + dw, dy);
    ctx.scale(-1, 1);
    ctx.drawImage(image, region.x, region.y, region.w, region.h, 0, 0, dw, dh);
    ctx.restore();
    return;
  }
  ctx.drawImage(image, region.x, region.y, region.w, region.h, dx, dy, dw, dh);
}

function baseline_px() {
  return (world_h - 2) * tile_px;
}

function lane_to_sprite_y(lane, sprite_h) {
  const jump_offset = lane === 1 ? tile_px * 2 : 0;
  return baseline_px() - jump_offset - sprite_h;
}

function active_player_region(frame_no, lane, moving) {
  if (lane === 1) {
    return atlas.mario_and_items.mario_jump;
  }
  if (moving) {
    return frame_no % 2 === 0
      ? atlas.mario_and_items.mario_walk_a
      : atlas.mario_and_items.mario_walk_b;
  }
  return atlas.mario_and_items.mario_idle;
}

function pull_state_snapshot() {
  const kinds_h = fns.state_kinds(state);
  const xs_h = fns.state_xs(state);
  const lanes_h = fns.state_lanes(state);
  const active_h = fns.state_active(state);

  return {
    kinds: runtime.read_slice_u8(kinds_h),
    xs: runtime.read_slice_u8(xs_h),
    lanes: runtime.read_slice_u8(lanes_h),
    active: runtime.read_slice_u8(active_h),
    player_x: decodeInt(fns.state_player_x(state)),
    player_lane: decodeInt(fns.state_player_lane(state)),
    jump_timer: decodeInt(fns.state_jump_timer(state)),
    score: decodeInt(fns.state_score(state)),
    lives: decodeInt(fns.state_lives(state)),
    frame_no: decodeInt(fns.state_frame(state)),
    active_count: decodeInt(fns.state_active_count(state)),
  };
}

function draw_background() {
  const sky = ctx.createLinearGradient(0, 0, 0, canvas.height);
  sky.addColorStop(0, "#8dd6ff");
  sky.addColorStop(1, "#d0f0ff");
  ctx.fillStyle = sky;
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  for (let x = 0; x < world_w; x += 1) {
    const dx = x * tile_px;
    draw_region(
      images.blocks,
      atlas.blocks.ground_brick_brown,
      dx,
      (world_h - 2) * tile_px,
      tile_px,
      tile_px,
    );
    draw_region(
      images.blocks,
      atlas.blocks.ground_brick_brown,
      dx,
      (world_h - 1) * tile_px,
      tile_px,
      tile_px,
    );
  }

  const qy = (world_h - 5) * tile_px;
  for (const qx of [10, 18, 30]) {
    draw_region(
      images.blocks,
      atlas.blocks.question_block_brown,
      qx * tile_px,
      qy,
      tile_px,
      tile_px,
    );
  }
}

function draw_world(snapshot) {
  draw_background();

  const moving = key_state.left || key_state.right;
  const player_region = active_player_region(snapshot.frame_no, snapshot.player_lane, moving);
  const player_w = 20;
  const player_h = 34;
  const player_x_px = snapshot.player_x * tile_px;
  const player_y_px = lane_to_sprite_y(snapshot.player_lane, player_h);
  draw_region(images.mario_and_items, player_region, player_x_px, player_y_px, player_w, player_h, key_state.left);

  for (let i = 0; i < entity_capacity; i += 1) {
    if (snapshot.active[i] !== 1) {
      continue;
    }
    const kind = snapshot.kinds[i];
    const lane = snapshot.lanes[i];
    const x = snapshot.xs[i];

    if (kind === 1) {
      const goomba_region = snapshot.frame_no % 2 === 0
        ? atlas.enemies.goomba_walk_a
        : atlas.enemies.goomba_walk_b;
      const w = 24;
      const h = 24;
      const y = lane_to_sprite_y(lane, h);
      draw_region(images.enemies, goomba_region, x * tile_px, y, w, h);
    } else if (kind === 2) {
      const w = 16;
      const h = 16;
      const y = lane_to_sprite_y(lane, h);
      draw_region(images.mario_and_items, atlas.mario_and_items.coin_small, x * tile_px, y, w, h);
    }
  }

  stats.textContent = `score=${snapshot.score} lives=${snapshot.lives} frame=${snapshot.frame_no} entities=${snapshot.active_count} jump_timer=${snapshot.jump_timer}`;
}

function tick_once() {
  const left = key_state.left ? 1 : 0;
  const right = key_state.right ? 1 : 0;
  const jump = key_state.jump ? 1 : 0;
  const event = fns.event_tick(encodeInt(left), encodeInt(right), encodeInt(jump));
  state = fns.apply_event(event, state);
}

function frame(now_ms) {
  const delta_ms = now_ms - last_time_ms;
  last_time_ms = now_ms;

  const step_ms = 1000 / ticks_per_second;
  tick_accum_ms = Math.min(tick_accum_ms + delta_ms, step_ms * max_catchup_ticks);
  let stepped = 0;
  while (tick_accum_ms >= step_ms && stepped < max_catchup_ticks) {
    tick_once();
    tick_accum_ms -= step_ms;
    stepped += 1;
  }

  const snapshot = pull_state_snapshot();
  draw_world(snapshot);
  requestAnimationFrame(frame);
}

function bind_controls() {
  const on_key = (down) => (event) => {
    if (event.code === "ArrowLeft" || event.code === "KeyA") {
      key_state.left = down;
      event.preventDefault();
      return;
    }
    if (event.code === "ArrowRight" || event.code === "KeyD") {
      key_state.right = down;
      event.preventDefault();
      return;
    }
    if (event.code === "Space" || event.code === "ArrowUp" || event.code === "KeyW") {
      key_state.jump = down;
      event.preventDefault();
    }
  };

  window.addEventListener("keydown", on_key(true));
  window.addEventListener("keyup", on_key(false));

  reset_btn.addEventListener("click", () => {
    state = fns.apply_event(fns.event_reset(encodeInt(0)), state);
  });
}

async function load_runtime() {
  const response = await fetch("../out/mario_ecs.wasm");
  if (!response.ok) {
    throw new Error(`failed to load ../out/mario_ecs.wasm: ${response.status}`);
  }
  const bytes = await response.arrayBuffer();
  const loaded = await instantiateWithRuntime(bytes);

  const world_width_fn = loaded.instance.exports.world_width;
  const world_height_fn = loaded.instance.exports.world_height;
  const entity_capacity_fn = loaded.instance.exports.entity_capacity;
  const init_state_fn = loaded.instance.exports.init_state;
  const apply_event_fn = loaded.instance.exports.apply_event;
  const event_tick_fn = loaded.instance.exports.event_tick;
  const event_reset_fn = loaded.instance.exports.event_reset;
  const state_kinds_fn = loaded.instance.exports.state_kinds;
  const state_xs_fn = loaded.instance.exports.state_xs;
  const state_lanes_fn = loaded.instance.exports.state_lanes;
  const state_active_fn = loaded.instance.exports.state_active;
  const state_player_x_fn = loaded.instance.exports.state_player_x;
  const state_player_lane_fn = loaded.instance.exports.state_player_lane;
  const state_jump_timer_fn = loaded.instance.exports.state_jump_timer;
  const state_score_fn = loaded.instance.exports.state_score;
  const state_lives_fn = loaded.instance.exports.state_lives;
  const state_frame_fn = loaded.instance.exports.state_frame;
  const state_active_count_fn = loaded.instance.exports.state_active_count;

  const required = [
    world_width_fn,
    world_height_fn,
    entity_capacity_fn,
    init_state_fn,
    apply_event_fn,
    event_tick_fn,
    event_reset_fn,
    state_kinds_fn,
    state_xs_fn,
    state_lanes_fn,
    state_active_fn,
    state_player_x_fn,
    state_player_lane_fn,
    state_jump_timer_fn,
    state_score_fn,
    state_lives_fn,
    state_frame_fn,
    state_active_count_fn,
  ];

  if (required.some((fn) => typeof fn !== "function")) {
    throw new Error("mario_ecs.wasm is missing required exports");
  }

  runtime = loaded.runtime;
  fns = {
    world_width: world_width_fn,
    world_height: world_height_fn,
    entity_capacity: entity_capacity_fn,
    init_state: init_state_fn,
    apply_event: apply_event_fn,
    event_tick: event_tick_fn,
    event_reset: event_reset_fn,
    state_kinds: state_kinds_fn,
    state_xs: state_xs_fn,
    state_lanes: state_lanes_fn,
    state_active: state_active_fn,
    state_player_x: state_player_x_fn,
    state_player_lane: state_player_lane_fn,
    state_jump_timer: state_jump_timer_fn,
    state_score: state_score_fn,
    state_lives: state_lives_fn,
    state_frame: state_frame_fn,
    state_active_count: state_active_count_fn,
  };

  world_w = decodeInt(fns.world_width(encodeInt(0)));
  world_h = decodeInt(fns.world_height(encodeInt(0)));
  entity_capacity = decodeInt(fns.entity_capacity(encodeInt(0)));

  canvas.width = world_w * tile_px;
  canvas.height = world_h * tile_px;

  const kinds = runtime.alloc_slice_u8(new Uint8Array(entity_capacity));
  const xs = runtime.alloc_slice_u8(new Uint8Array(entity_capacity));
  const lanes = runtime.alloc_slice_u8(new Uint8Array(entity_capacity));
  const active = runtime.alloc_slice_u8(new Uint8Array(entity_capacity));

  state = fns.init_state(kinds, xs, lanes, active);
  state = fns.apply_event(fns.event_reset(encodeInt(0)), state);
}

async function boot() {
  try {
    images = {
      blocks: await load_image("./assets/blocks.png"),
      enemies: await load_image("./assets/enemies.png"),
      mario_and_items: await load_image("./assets/mario_and_items.png"),
    };

    await load_runtime();
    bind_controls();
    last_time_ms = performance.now();
    requestAnimationFrame(frame);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    stats.textContent = `error: ${message}`;
    console.error(error);
  }
}

boot();
