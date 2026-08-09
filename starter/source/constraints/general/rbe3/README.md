# RBE3 Interpolation Element (`starter/source/constraints/general/rbe3/`)

Reads /RBE3: an interpolation constraint that distributes the motion of a master node as a weighted average of slave node motions (force/moment redistribution without rigid-body stiffness addition).

## Key Files

| File | Role |
|------|------|
| `hm_preread_rbe3.F` | Pre-read: count /RBE3 blocks, allocate |
| `hm_read_rbe3.F` | Parse /RBE3 card: master node, weighted slave set, active DOFs |
| `contrbe3.F` | Compute RBE3 constraint coefficients from weighted average |

## Description

Unlike RBE2, RBE3 does not add stiffness — it defines the master node motion as a weighted average of slave motions. `contrbe3.F` computes the interpolation coefficients from the slave-node weights and positions. Used for attaching loads or sensors to flexible structures without artificially stiffening the model.

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/rbe3/README.md` — engine-side enforcement
