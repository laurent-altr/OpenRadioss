# Mullins Effect Failure Criterion (`engine/source/materials/fail/mullins_or/`)

Failure criterion for rubber/elastomer materials exhibiting the Mullins effect: stress-softening under cyclic loading.

## Key Files

| File | Role |
|------|------|
| `fail_mullins_OR_s.F` | Mullins-effect failure check for shell elements |

## Model

The Mullins effect is a progressive stress-softening in filled elastomers under repeated cycling. The criterion tracks the maximum strain energy `W_max` attained in loading history. Failure occurs when the current damage-modified strain energy exceeds the fracture threshold:

```
W_damage = η(W_max) × W_current ≥ W_fail
```

where `η(W_max) = 1 − (1/r) erf(W_max / (m + β W_max))` is the Ogden-Roxburgh damage function. Used for rubber seals, gaskets, and elastomeric mounts under cyclic fatigue loading.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/energy/README.md` — energy-based failure
