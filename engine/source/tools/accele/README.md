# Engine Accelerometer (`engine/source/tools/accele/`)

Computes accelerometer output: filtered nodal accelerations in a moving local frame for injury criterion evaluation.

## Key Files

| File | Role |
|------|------|
| `accel1.F` | Main accelerometer: compute filtered acceleration at a node in local frame |
| `cutcnt.F` | Count CFC (Channel Frequency Class) filter coefficients |
| `cutcon.F` | Construct CFC Butterworth filter coefficients |
| `cutfunc.F` | Apply CFC filter to acceleration signal |
| `cutfunce.F` | Extended CFC filter for end-of-step correction |
| `cutmain.F` | Main CFC filter driver |
| `butterworth.F` | Butterworth filter implementation |

## Algorithm

Accelerometers filter raw nodal accelerations using SAE/CFC (Channel Frequency Class) low-pass Butterworth filters (CFC60, CFC180, CFC600, CFC1000). `cutcon.F` computes bilinear-transform digital filter coefficients at startup. `cutfunc.F` applies the recursive filter each step. `accel1.F` also transforms the acceleration from global to the accelerometer's local (moving) reference frame for dummy-based injury criteria.

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `starter/source/tools/accele/README.md` — accelerometer definition in starter
