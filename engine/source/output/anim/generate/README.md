# Animation Writer (`engine/source/output/anim/generate/`)

Generates animation result data (nodal and element fields) written to the legacy Radioss `.Annn` animation files.

## Key Files

| File | Role |
|------|------|
| `genani.F` | Main animation generation entry: loop over time steps, call result writers |
| `anicon0.F` / `anicon2.F` | Animation connectivity output (element topology) |
| `animx.F` | Coordinate output (deformed nodal positions) |
| `animig3d.F` | IGA/NURBS element animation output |
| `tensor0.F` / `tensor6.F` / `tensorc.F` / `tensorc_crk.F` / `tensorc_ply.F` | Stress tensor output per element type |
| `xyznod.F` / `xyznod_crk.F` / `xyznod_ply.F` | Nodal coordinate output (crack / ply variants) |
| `xyznor.F` / `xyznor_crk.F` / `xyznor_ply.F` | Nodal normal vector output |
| `nodald.F` | Nodal displacement output |
| `nodalp.F` | Nodal pressure output |
| `nodalt.F` | Nodal temperature output |
| `nodaldt.F` | Nodal time-step output |
| `nodalssp.F` | Nodal specific sound speed output |
| `nodalvfrac.F` | Nodal volume fraction output (ALE) |
| `nodalvol.F` | Nodal volume output |
| `nodalzvol.F` | Nodal Z-volume output |
| `nodal_schlieren.F` | Schlieren (density gradient) output |
| `output_div_u.F` | Divergence of velocity output (CFD) |
| `output_schlieren.F` | Schlieren field output |
| `sigrota.F` / `sigrota_xfe.F` | Stress rotation for output |
| `srota6.F` / `srota6_s8s.F` | Stress rotation for 6-DOF / SANSB elements |
| `qrota3.F` / `qrota_group.F` / `qrota_vect.F` | Quaternion rotation for animation |
| `velvec.F` / `velvecz22.F` | Velocity vector output |
| `torseur.F` | Force/moment resultant (torseur) output |
| `scanor.F` | Section normal output |
| `facnor.F` | Face normal computation for animation |
| `eigoff.F` | Eigenvalue output for modal animation |
| `tensgpstrain.F` | Gauss-point strain tensor output |
| `ani_pcont.F` | Pressure contact animation output |
| `ani_txt.F` | ASCII text animation format writer |
| `anim_crk_init.F` | XFEM crack animation initialisation |
| `anim_nodal_contour_fvmbags.F` | FVM airbag nodal contour output |
| `anim_nodal_p_elems.F` | Nodal pressure from elements output |
| `anim_nodal_ssp_elems.F90` | Nodal sound speed from elements output |
| `anim_nodal_vector_fvmbags.F` | FVM airbag nodal vector output |
| `anin_ply_init.F` | Ply animation initialisation |
| `aninflow.F` | Inflow boundary animation output |
| `anioff0.F` / `anioff6.F` / `anioffc.F` | Animation offset writers |
| `anioffc_crk.F` / `anioffc_ply.F` / `aniofff.F` | Crack/ply/full animation offset writers |
| `aniskew.F` / `aniskewf.F` | Skew frame animation output |
| `delnumb0.F` / `delnumb6.F` | Delete element numbering for animation |
| `delnumbc.F` / `delnumbc_crk.F` / `delnumbc_ply.F` / `delnumbf.F` | Delete numbering variants |
| `dfunc0.F` / `dfunc6.F` / `dfuncc.F` / `dfuncc_crk.F` / `dfuncc_ply.F` / `dfuncf.F` | Damage function output |
| `dmasani0.F` / `dmasani6.F` / `dmasanic.F` / `dmasanif.F` | Mass animation output |
| `dnwalc.F` / `dnwals.F` / `dxwalc.F` / `dxwall.F` / `dxwalp.F` / `dxwals.F` | Rigid wall animation output |
| `donerbe2.F` / `donerbe3.F` / `donerby.F` | RBE2/RBE3/rigid body animation output |
| `donerwl.F` / `donesec.F` / `donesrg.F` | Rigid wall / section / sub-group animation |
| `dparrbe2.F` / `dparrbe3.F` / `dparrby.F` / `dparrws.F` / `dparsrg.F` | Parallel animation gather routines |
| `drbe2cnt.F` / `drbe3cnt.F` / `drbycnt.F` / `dseccnt.F` | Count animation entities |
| `dsecnor.F` / `dsphcnt.F` / `dsphnor.F` / `dsrcnt.F` / `dsrgnor.F` | Section/SPH animation output |
| `dxyzsect.F` / `dxyzsph.F` / `dxyzsrg.F` | XYZ coordinates for section/SPH/sub-group |
| `forani1.F` / `forani2.F` / `forani3.F` | Force animation (1D, 2D, 3D elements) |
| `monvol_anim.F90` | Monolithic airbag (CV) animation output |
| `parsor0.F` / `parsor_crk.F` / `parsor_ply.F` / `parsorc.F` / `parsorf.F` / `parsors.F` | Parser for animation sort order |
| `schlieren_buffer_gathering.F` | Schlieren data gather across SPMD domains |
| `xanim29.F` / `xanim30.F` / `xanim31.F` | XFEM crack animation writers |
| `xfeconnec3n.F` / `xfeconnec4n.F` | XFEM element connectivity for animation |
| `xfecut.F` | XFEM crack-cut surface output |

## Related Documentation

- `engine/source/output/anim/README.md` — parent directory
- `engine/source/output/anim/reader/README.md` — animation file reader
- `engine/source/output/h3d/README.md` — H3D output (Altair binary format)
