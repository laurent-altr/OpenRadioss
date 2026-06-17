# Failure Wave Propagation (`engine/source/materials/fail/failwave/`)

Models failure wave propagation through brittle materials: a sharp failure front advances at a prescribed speed behind a shock wave.

## Key Files

| File | Role |
|------|------|
| `set_failwave_nod3.F` | Initialise failure wave front nodes (triangular shells) |
| `set_failwave_nod4.F` | Initialise failure wave front nodes (quadrilateral shells) |
| `upd_failwave_sh3n.F` | Advance failure wave through triangular shell mesh |
| `upd_failwave_sh4n.F` | Advance failure wave through quadrilateral shell mesh |
| `update_failwave.F` | Master update: advance failure wave front each step |
| `seg_intersect.F` | Segment intersection utility for wave-front geometry |

## Algorithm

A failure wave propagates as a geometric front at user-defined speed `c_fail`. Each step:
1. `update_failwave.F` advances the front by `c_fail × Δt`
2. Shell elements whose centroid is behind the front are marked as failed
3. `upd_failwave_sh3n/4n.F` handles the mesh topology of the advancing front for triangular and quadrilateral shell elements respectively

Used for glass and ceramic plates under ballistic impact where a damage wave is observed to propagate behind the initial stress wave.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/alter/README.md` — Alter criterion (wave-related)
