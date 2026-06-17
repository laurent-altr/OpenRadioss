# Starter Groups Subsystem

This subsystem reads all group and set keyword definitions and validates their membership.

## Key Files

| File | Role |
|------|------|
| `check_surf.F` | Validate surface group (check for zero-area segments, orientation consistency) |
| `elegror.F` | Element group error reporting |
| `elegror_seatbelt.F` | Error reporting for seatbelt element groups |
| `groups_get_elem_list.F` | Retrieve element list from any group type (unified interface) |
| `groups_get_nentity.F` | Get number of entities in a group |

## Group Types Handled

| Keyword | Type | Description |
|---------|------|-------------|
| `/GRNOD` | Node group | Collection of node IDs |
| `/GRSH` | Shell element group | Collection of shell element IDs |
| `/GRSO` | Solid element group | Collection of solid element IDs |
| `/GRBE` | Beam element group | Collection of beam element IDs |
| `/GRSP` | Spring element group | Collection of spring element IDs |
| `/GRTRIA` | Triangular shell group | Collection of tri-shell element IDs |
| `/GRPART` | Part group | All elements with a given property ID |
| `/SURF` | Surface (segment set) | Collection of element faces / segments |
| `/LINE` | Line (edge set) | Collection of element edges |

## Validation

After all groups are read, the starter validates:
- All referenced entity IDs exist in the model
- Surface segments have consistent outward normals (for contact surfaces)
- Part groups reference existing property IDs

Errors and warnings are issued for missing entities or inconsistent orientations.

## Related Documentation

- `engine/source/groups/README.md` — runtime group data usage (engine)
- `starter/source/model/README.md` — group resolution in model assembly
