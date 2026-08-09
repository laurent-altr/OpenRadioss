# Bounding Box Utilities (`starter/source/model/box/`)

Computes and manages bounding boxes for nodes, elements, and the global model. Used for spatial queries and contact sorting.

## Key Files

| File | Role |
|------|------|
| `bigbox.F` | Compute global bounding box of all nodes in the model |
| `hm_bigbox.F` / `hm_bigbox2.F` | HM binary path for computing global bounding box |
| `boxbox.F` | Test whether two axis-aligned bounding boxes (AABB) overlap |
| `rdbox.F` | Read `/BOX` keyword: user-defined rectangular bounding box |
| `read_box_box.F` | Read `BOX/BOX` (axis-aligned box defined by two corner points) |
| `read_box_cyl.F` | Read `BOX/CYL` (cylindrical bounding volume) |
| `read_box_rect.F` | Read `BOX/RECT` (rectangular box) |
| `read_box_spher.F` | Read `BOX/SPHER` (spherical bounding volume) |
| `nboxlist.F` | Build node list: find all nodes inside a given `/BOX` |
| `hm_read_box.F` | HM binary reader for `/BOX` definitions |

## Usage

Bounding boxes are used for:
- Contact broad-phase: test element bounding boxes for overlap before expensive narrow-phase check
- Group definitions: `GRNOD/BOX` selects all nodes inside a `/BOX` volume
- Initial conditions: `INIVEL/BOX` applies velocity to nodes inside a box

`bigbox.F` computes the overall model extents — used to set the bucket size for spatial hash tables in contact sorting.

## Related Documentation

- `starter/source/model/README.md` — parent model directory
- `starter/source/interfaces/README.md` — contact sorting uses bounding boxes
