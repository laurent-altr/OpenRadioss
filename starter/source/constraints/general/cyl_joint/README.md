# Cylindrical Joint (`starter/source/constraints/general/cyl_joint/`)

Reads and initialises /CYL_JOINT: cylindrical joint between two rigid bodies (one translational + one rotational DOF along the joint axis).

## Key Files

| File | Role |
|------|------|
| `hm_prelecjoi.F` | Pre-read: count joint definitions |
| `hm_read_cyljoint.F` | Parse /CYL_JOINT card: axis nodes, direction |
| `init_joint.F` | Initialise joint state and constraint matrices |
| `split_joint.F` | Distribute joint data across SPMD domains |
| `write_joint.F` | Write joint data to restart file |
| `write_count_joint_sms.F` | Write joint count for SMS (mass scaling) |
| `deallocate_joint.F` | Free joint data structures |

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/cyl_joint/README.md` — engine-side joint enforcement
