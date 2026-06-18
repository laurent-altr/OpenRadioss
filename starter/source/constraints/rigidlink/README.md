# Rigid Link (`starter/source/constraints/rigidlink/`)

Reads /RLINK (rigid link) constraints that tie slave nodes to a master node with a rigid kinematic relationship.

## Key Files

| File | Role |
|------|------|
| `hm_pre_read_rlink.F` | Pre-read: count /RLINK blocks |
| `hm_read_rlink.F` | Parse /RLINK card: master node, slave node set, DOF mask |
| `hm_read_spcnd.F` | Parse /SPCND (single point constraint node): fixed DOF on node |

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/rlink/README.md` — engine-side rlink enforcement
