# Sprite Regions (Prototype Atlas Notes)

These regions are practical, approximate picks for the local Mario-like prototype.
Coordinates use top-left origin `(x, y)` in pixels.

## Sheet summary

- `blocks.png`: `400 x 240` RGBA, mostly 16px tile motifs with color-row variants.
- `enemies.png`: `702 x 252` RGBA, mostly 36px cell grid with per-row palette variants.
- `mario_and_items.png`: `360 x 162` RGBA, small-Mario strip plus item strip.

## `blocks.png`

Likely structure:
- Left cluster: tile motifs and block variants.
- Middle/right: pipe and terrain segments in larger chunks.

Prototype regions:

| name | x | y | w | h | notes |
| --- | ---: | ---: | ---: | ---: | --- |
| `ground_brick_brown` | 0 | 0 | 16 | 16 | main ground tile used by demo |
| `question_block_brown` | 95 | 0 | 16 | 16 | optional question block accent |
| `pipe_cap_brown` | 188 | 0 | 36 | 32 | optional decor (not required for gameplay) |

## `enemies.png`

Likely structure:
- Horizontal rows with repeated palette variants.
- 36x36-ish enemy cells across the row.

Prototype regions:

| name | x | y | w | h | notes |
| --- | ---: | ---: | ---: | ---: | --- |
| `goomba_walk_a` | 0 | 0 | 36 | 36 | default enemy frame |
| `goomba_walk_b` | 36 | 0 | 36 | 36 | alternate frame |
| `koopa_green` | 72 | 0 | 36 | 36 | optional enemy variant |

## `mario_and_items.png`

Likely structure:
- Top strips: small/big/fire Mario animation frames.
- Mid strip (`y ~ 72`): items (coins, mushrooms, stars, shells).

Prototype regions:

| name | x | y | w | h | notes |
| --- | ---: | ---: | ---: | ---: | --- |
| `mario_idle` | 18 | 2 | 18 | 34 | player idle frame |
| `mario_walk_a` | 36 | 2 | 18 | 34 | walk cycle frame A |
| `mario_walk_b` | 54 | 2 | 18 | 34 | walk cycle frame B |
| `mario_jump` | 90 | 2 | 16 | 34 | jump frame |
| `coin_small` | 1 | 73 | 15 | 17 | collectible used by demo |
| `mushroom_red` | 36 | 72 | 18 | 18 | optional power-up render |
| `star_small` | 4 | 149 | 7 | 8 | tiny HUD-scale icon |

## Reliability note

These values are intentionally practical for quick prototyping and may need 1-3px nudges for perfect atlas alignment in some frames.
