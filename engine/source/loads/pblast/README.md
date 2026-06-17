# Pressure Blast Load (`engine/source/loads/pblast/`)

Applies blast/explosion pressure-time histories to structural surfaces using empirical blast models (`/LOAD/PBLAST`).

## Key Files

| File | Role |
|------|------|
| `pblast.F` | Main blast pressure application: dispatch to sub-models |
| `pblast_1.F` | Blast model 1: user-defined pressure vs. time curve applied to segments |
| `pblast_2.F` | Blast model 2: spherical free-air burst with Kingery-Bulmash scaling |
| `pblast_3.F` | Blast model 3: hemispherical surface burst with ground reflection factor |

## Algorithm

For models 2 and 3, the Kingery-Bulmash polynomial fit computes peak overpressure `P_peak`, positive phase duration `t_d`, and impulse `I` as functions of scaled distance `Z = R / W^(1/3)` (distance / charge-weight cube-root). The Friedlander waveform:

```
P(t) = P_peak × (1 − t/t_d) × exp(−b t/t_d)
```

is then applied to each segment, accounting for the arrival time `t_a = R/c`. For model 3 a ground-reflection enhancement factor is added. Segment-face areas and normals are recomputed if the mesh deforms significantly.

## Related Documentation

- `engine/source/loads/README.md` — parent loads directory
- `common_source/modules/loads/README.md` — pblast_mod data structures
