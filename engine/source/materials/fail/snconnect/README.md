# Shell-Node Connector Failure Criterion (`engine/source/materials/fail/snconnect/`)

Failure criterion for shell-to-node connectors (spot-weld/adhesive represented as rigid links between shell layers).

## Key Files

| File | Role |
|------|------|
| `fail_snconnect.F` | Shell-node connector failure: peel + shear force interaction |

## Criterion

Spot-weld failure criterion in peel (`F_n`) and shear (`F_s`) force space:

```
(F_n / F_n_max)^a + (F_s / F_s_max)^b ≥ 1
```

Tension-only in the normal direction (peel force, `F_n > 0` only). At failure the rigid connection is released, allowing the connected shell nodes to separate. Used for resistance spot welds and structural adhesive bonds in vehicle body-in-white crash models.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/connect/README.md` — generic connector failure
