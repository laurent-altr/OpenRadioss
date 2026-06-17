# Beam Element Initialisation (`starter/source/elements/beam/`)

Reads and initialises beam elements (TYPE3 Euler-Bernoulli / Timoshenko beam and TYPE18 integrated beam).

## Key Files

| File | Role |
|------|------|
| `pinit3.F` | Main initialisation for TYPE3 beam: read connectivity, set up cross-section frames |
| `pcoori.F` | Compute initial beam local coordinate frame from node positions |
| `peveci.F` | Compute beam eigenvectors (principal axes of the cross-section) |
| `pibuf3.F` | Populate beam element buffer (`ELBUF`) with geometric and material data |
| `pmass.F` | Compute beam mass: lumped translational + rotational nodal contributions |
| `pgrhead.F` / `pgrtails.F` | Write beam element group header/tail to restart file |
| `bsigini.F` | Read initial stress state for beam elements (`/INISTA`) |
| `buserini.F` | Initialise user-defined beam (LAW99 path) |
| `dt1lawp.F` | Estimate critical time step for TYPE3 beams from wave speed and element length |

## Beam Types

| Keyword | Type | Formulation |
|---------|------|-------------|
| `/BEAM` TYPE3 | Euler-Bernoulli / Timoshenko 2-node beam | Full cross-section integration |
| `/BEAM` TYPE18 | Integrated Euler beam with arbitrary cross-section | Warping DOFs |

## Initialisation Steps

1. Read connectivity (node IDs, property ID, material ID)
2. Compute local frame: axial, bending, and torsional axes (`pcoori.F`, `peveci.F`)
3. Allocate and fill element buffer (`pibuf3.F`)
4. Compute lumped nodal mass (`pmass.F`)
5. Estimate element time-step contribution (`dt1lawp.F`)
6. Write to restart group record

## Related Documentation

- `engine/source/elements/beam/README.md` — engine-side force computation
- `starter/source/elements/README.md` — parent directory
- `starter/source/properties/beam/` — cross-section property types
