# Engine Report Output (`engine/source/output/report/`)

Writes summary report files at end of run: energy balance, timing statistics, and run summary.

## Key Files

| File | Role |
|------|------|
| `report.F` | Write global energy/timing summary report at end of run |
| `sortie_mvw.F` | Write mass/volume/weight totals to report |

## Contents

`report.F` produces the end-of-run report including:
- Total CPU time and wall time
- Energy balance: internal energy, kinetic energy, contact energy, hourglass energy, artificial viscosity energy
- Mass added by mass scaling (if any)
- Momentum balance check
- Interface (contact) statistics: max penetration, min/max gap

`sortie_mvw.F` computes and reports the total model mass, volume, and weight at the current step.

## Related Documentation

- `engine/source/output/README.md` — all output types
- `starter/source/output/qaprint/README.md` — pre-run QA print (starter)
