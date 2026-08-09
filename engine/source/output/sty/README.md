# STY Output (`engine/source/output/sty/`)

Writes `.sty` (style/extended state) files: an additional per-element layer for specialist output quantities not included in standard STA frames.

## Key Files

| File | Role |
|------|------|
| `genoutp.F` | Master STY writer: dispatch per element-type output |
| `outp_c_s.F` / `outp_c_t.F` | Solid element scalar/tensor output |
| `outp_s_s.F` / `outp_s_t.F` | Shell element scalar/tensor output |
| `outp_r_s.F` / `outp_r_t.F` | Beam (rod) scalar/tensor output |
| `outp_sp_s.F` / `outp_sp_t.F` | SPH scalar/tensor output |
| `outp_n_v.F` | Nodal vector output |
| `outp_no.F` | Nodal scalar output |
| `outp_mt.F` | Material-specific custom output |
| `s_user.F` | User-defined output variables |
| `c_tf_ne.F` | Transform to normal/tangential coordinates |

## Related Documentation

- `engine/source/output/README.md` — all output types
- `engine/source/output/sta/README.md` — standard STA state output
