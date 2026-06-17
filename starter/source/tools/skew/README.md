# Skew Frames (`starter/source/tools/skew/`)

Reads and initialises local reference frames (`/SKEW`, `/FRAME`). Skew frames define local coordinate systems for boundary conditions, loads, output, and node-specific orientation.

## Key Files

| File | Role |
|------|------|
| `hm_read_skw.F` | HM binary reader for `/SKEW` definitions |
| `hm_preread_skw.F90` | Pre-pass: count `/SKEW` entries from HM binary |
| `hm_read_frm.F` | HM binary reader for `/FRAME` (moving reference frame) definitions |

## Skew vs Frame

| Keyword | Type | Description |
|---------|------|-------------|
| `/SKEW/FIX` | Fixed | Constant local axes (X, Y, Z directions given) |
| `/SKEW/MOV` | Moving | Axes follow a rigid body motion |
| `/FRAME/MOV` | Moving frame | Full moving reference frame (Euler angles updated each step) |
| `/FRAME/FIX` | Fixed frame | Fixed orientation, possibly offset position |

Skew frames are referenced by ID from boundary conditions (`/BCS`), loads (`/CLOAD`, `/PLOAD`), and output requests (`/TH`, `/ANIM`).

## Engine Usage

The starter writes skew frame definitions to the restart file. The engine reads them during initialisation (`resol_init.F`) and evaluates moving frames each time step in `movfram.F` / `newskw.F` (in `engine/source/tools/`).

## Related Documentation

- `engine/source/tools/README.md` — engine frame update (`movfram.F`, `newskw.F`)
- `starter/source/tools/README.md` — parent tools directory
