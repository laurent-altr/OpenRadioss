# Beam Element Force Computation (`engine/source/elements/beam/`)

Computes internal forces and moments for TYPE3 (Euler-Bernoulli/Timoshenko) and TYPE18 (integrated) beam elements each time step.

## Key Files

| File | Role |
|------|------|
| `main_beam3.F` | Main TYPE3 beam force loop: calls geometry, material, and assembly routines |
| `main_beam18.F` | Main TYPE18 beam force loop |
| `pfint3.F` | Compute internal force vector for TYPE3 beam |
| `pmat3.F` | Compute beam material response (stress resultants from curvature/extension) |
| `pdefo3.F` | Compute beam deformation measures (extension, curvature, twist) |
| `pcoor3.F` | Update beam coordinate frame (co-rotational update) |
| `pcoork3.F` | Compute beam kinematics in updated frame |
| `pcurv3.F` | Compute beam curvature from nodal rotations |
| `pevec3.F` | Compute beam local axis vectors |
| `pforc3.F` | Scatter beam forces/moments to global force vector |
| `pdamp3.F` | Beam structural damping contribution |
| `pke3.F` | Beam kinetic energy computation |
| `ppxpy3.F` | Compute beam cross-section stress resultants (N, My, Mz, T) |
| `psumg3.F` | Accumulate beam contributions to global arrays |
| `pfcum3.F` / `pfcum3p.F` | Accumulate forces (serial / parallel OpenMP paths) |
| `pmcum3.F` / `pmcum3p.F` | Accumulate moments (serial / parallel paths) |
| `pdlen3.F` | Compute current beam length |
| `peoff.F` | Handle beam eccentric offset (beam centre line offset from node) |
| `pbilan.F` | Beam energy balance (internal energy, hourglass) |
| `mulaw_ib.F` | Material law dispatch for integrated beam (TYPE18) |
| `fail_beam3.F` | Beam failure criteria evaluation |
| `fail_beam18.F` | TYPE18 beam failure criteria |
| `thermexpp.F` | Thermal expansion contribution to beam internal force |

## Co-rotational Formulation

TYPE3 beams use a co-rotational approach:
1. Extract rigid body rotation from nodal displacements (`pcoor3.F`)
2. Compute deformation in the local (body-attached) frame (`pdefo3.F`, `pcurv3.F`)
3. Evaluate constitutive law — stress resultants from strain measures (`pmat3.F`)
4. Transform forces/moments back to global frame and scatter (`pforc3.F`)

This handles large rotations correctly while keeping the constitutive law simple.

## Related Documentation

- `starter/source/elements/beam/README.md` — beam initialisation
- `engine/source/elements/README.md` — parent elements directory overview
- `starter/source/properties/beam/` — cross-section and material parameters
