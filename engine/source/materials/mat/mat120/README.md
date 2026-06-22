# LAW120 — Elastic-Plastic with Multiple Yield Criteria (`engine/source/materials/mat/mat120/`)

Elastic-plastic material supporting Von Mises, Drucker-Prager, Cazacu-Barlat
asymmetric, and tabulated yield criteria; solid and connected-element variants.

## Key Files

| File | Role |
|------|------|
| `sigeps120.F` | Main dispatcher: selects criterion from IEOS flag |
| `sigeps120_vm.F` / `sigeps120_dp.F` | Von Mises / Drucker-Prager variants |
| `sigeps120_tab_vm.F` / `sigeps120_tab_dp.F` | Tabulated VM / DP variants |
| `sigeps120_connect_main.F` | Connected-element main entry |
| `sigeps120_connect_vm.F` / `sigeps120_connect_dp.F` | Connected VM / DP |
| `sigeps120_connect_tab_vm.F` / `sigeps120_connect_tab_dp.F` | Connected tabulated |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
