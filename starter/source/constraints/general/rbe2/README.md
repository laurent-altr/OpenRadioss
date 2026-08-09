# RBE2 Rigid Spider (`starter/source/constraints/general/rbe2/`)

Reads /RBE2: a rigid spider element that ties multiple slave nodes to a single master node with full kinematic coupling.

## Key Files

| File | Role |
|------|------|
| `hm_read_rbe2.F` | Parse /RBE2 card: master node, slave node set, active DOFs |

## Description

RBE2 enforces a rigid kinematic relationship between all slave nodes and the master node: `u_slave = u_master + θ_master × r`. All slave DOFs listed in the constraint mask move rigidly with the master. Commonly used to represent rigid connections (e.g., bolt head to surrounding shell nodes, rigid stiffener attachments).

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/rbe2/README.md` — engine-side enforcement
