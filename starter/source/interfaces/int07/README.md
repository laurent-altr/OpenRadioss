# TYPE7 Interface — General Node to Surface (`starter/source/interfaces/int07/`)

Starter reader for /INTER/TYPE7: the most widely used general-purpose node-to-surface contact in Radioss.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type07.F` | Parse /INTER/TYPE7 card parameters |
| `hm_read_inter_lagdt_type07.F` | Parse Lagrangian DT (nodal time-step) parameters for TYPE7 |
| `hm_read_inter_lagmul_type07.F` | Parse Lagrange multiplier options for TYPE7 |

## Description

TYPE7 implements penalty-based node-to-surface contact. Slave nodes are checked against master surface segments each step; if penetration exceeds a gap threshold, a penalty restoring force is applied. The starter reads the master segment set (shells, solids, rigid walls), slave node set, gap/stiffness, friction model, and search radius. Penalty stiffness can be set explicitly or computed from material/element stiffness. The `lagdt` variant also applies a contact-based nodal time-step restriction.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/interf1/README.md` — contact geometry setup
- `engine/source/interfaces/README.md` — runtime contact enforcement
