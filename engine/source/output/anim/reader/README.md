# Animation File Reader (`engine/source/output/anim/reader/`)

Reads animation output control data from the restart file at engine startup: which variables to write, output frequencies, and animation format flags.

## Key Files

| File | Role |
|------|------|
| `anim_build_index_all.F` | Build index arrays for all animation output variables |
| `anim_dcod_key_0.F` | Decode animation keyword flags from restart |
| `anim_reset_index_all.F` | Reset animation index arrays to initial state |
| `anim_set2zero_struct.F` | Zero-initialise animation control structure |
| `freanim.F` | Read animation control block from restart file |

## Description

At engine startup, `freanim.F` reads the animation control block written by the starter (variable selectors, frequencies, format flags). `anim_build_index_all.F` translates the variable flags into per-variable index arrays used by `genani.F` to dispatch the correct writer for each requested result field.

## Related Documentation

- `engine/source/output/anim/README.md` — parent directory
- `engine/source/output/anim/generate/README.md` — animation data writers
