# 3D Interface Geometry (`starter/source/interfaces/inter3d1/`)

3D contact geometry initialisation: the largest and most complex interface setup module. Builds all contact data structures used by the engine during the simulation.

## Key Files

| File | Role |
|------|------|
| `inint3.F` | Main 3D interface initialisation orchestrator |
| `inint3_thkvar.F` | Thickness-variation aware initialisation |
| `inintmass.F` | Contact mass scaling (nodal mass perturbation for DT control) |
| `insol3.F` | Solid face initialisation for contact master surfaces |
| `insolt10.F` | Tet10 face initialisation for contact |
| `incoq3.F` | Shell face initialisation (orientation, area, thickness offset) |
| `inelt.F` | Element type dispatcher for surface initialisation |
| `iniend.F` | Finalise interface data structures |
| `inist3.F` | Compute initial penetration and gap state |
| `invoi3.F` | Build master segment neighbour (adjacency) lists |
| `inintr.F` | Initial search: assign slave nodes to master segments |
| `inintr1.F` | Slave node initial assignment (local) |
| `inintr2.F` | Slave node initial assignment (global after SPMD) |
| `inintr_thkvar.F` | Thickness-variable slave node assignment |
| `inintr_orthdirfric.F` | Orthotropic friction direction initialisation |
| `inintsub.F` | Sub-contact segment processing |
| `norma1.F` | Compute averaged nodal normals on master surface |
| `volint.F` | Interior/exterior test for 3D contact |
| `invoi3.F` | Master segment neighbour list construction |
| `voisin1.F` | Neighbour traversal utility |
| `i7buc1.F`, `i7buc_vox1.F` | TYPE7 voxel broad-phase initialisation |
| `i25buc_vox1.F`, `i25buce_edg.F` | TYPE25 voxel + edge broad-phase initialisation |
| `i2trivox.F90` | Voxel grid construction for broad-phase contact search |
| `inter_save_candidate.F90` | Save initial candidate pair list to restart |
| `inpoint.F` | Point-in-polygon test for contact surface |
| `insurf.F`, `insurf23.F`, `insurf_dx.F` | Surface segment area and DX computations |
| `insurfigeo.F` | Isogeometric surface initialisation (IGA contact) |
| `ingrbric.F` | Solid brick surface initialisation |
| `ingrbric_centroids.F` | Solid centroid computation for contact |
| `ingrbric_nodes.F` | Solid face node extraction |
| `lecins.F`, `lecint.F` | Read interface data from restart |
| `lecstamp.F` | Stamping interface initialisation |
| `count3.F`, `countigeo.F` | Count contact candidates |
| `select_s2s.F90` | Surface-to-surface candidate selection |
| `margin.F90` | Safety margin for gap computation |
| `reset_gap.F` | Reset gap state to initial value |
| `stifint_icontrol.F90` | Interface stiffness control flags |
| `upgrade_ixint.F`, `upgrade_remnode.F` | Contact data upgrade from older restart files |
| `io_type24.F` | TYPE24 restart I/O |
| `itagsl2.F` | Tag slave nodes for distributed memory |
| `intbufscratch_mod.F` | Scratch buffer module for contact |
| `definter.F` | Apply /DEF_INTER defaults |
| `chktyp2.F` | Check contact type compatibility |
| `scrint.F`, `prescrint.F` | Stamping contact (STAMP) initialisation |
| `presegmt.F`, `nsegmt.F` | Segment count and pre-processing |
| `trintfric.F` | Triangular friction element initialisation |
| `intbuf_fric_copy.F` | Copy friction buffer between contact types |
| `local_index.F` | Local-to-global index mapping |
| `i2master.F` | TYPE2 master surface indexing |
| `w_bufbric_22.F` | TYPE22 brick buffer write |

## Description

`inint3.F` is called once per contact interface during starter setup. The initialisation flow is:
1. Build the master surface (normals, areas, neighbour list).
2. Voxel grid construction for the broad-phase contact search (`i2trivox.F90`, `i7buc_vox1.F`).
3. Initial slave node assignment: find nearest master segment for each slave node (`inintr.F`).
4. Compute initial penetration and gap (`inist3.F`).
5. Write all contact data to the restart file for the engine.

## Related Documentation

- `starter/source/interfaces/inter2d1/README.md` — 2D contact geometry
- `starter/source/interfaces/interf1/README.md` — top-level dispatcher
- `engine/source/interfaces/README.md` — runtime contact enforcement
