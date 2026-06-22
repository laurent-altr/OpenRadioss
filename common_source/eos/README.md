# Equation of State (EOS) Models

This directory implements all Equation of State models shared between the starter and engine. EOS models relate pressure to density and internal energy for materials under high-rate loading (explosives, impacts, hydrodynamics).

## What is an EOS?

A standard material law computes stress from strain. An EOS provides the **pressure** as a function of current density `ρ` and specific internal energy `e`:

```
P = P(ρ, e)    or equivalently    P = P(V, T)
```

EOS models are used in combination with a strength model (material law) in high-velocity impact and explosive-loading simulations. They are referenced by `/EOS/<type>` keywords.

## EOS Models

| File | EOS Type | Description |
|------|----------|-------------|
| `gruneisen.F` | `/EOS/GRUNEISEN` | Mie-Grüneisen — for metals under shock loading |
| `jwl.F` | `/EOS/JWL` | Jones-Wilkins-Lee — detonation products expansion |
| `tillotson.F` | `/EOS/TILLOTSON` | Tillotson — vaporisation and cavitation of metals |
| `idealgas.F` | `/EOS/IDEAL-GAS` | Perfect ideal gas: P = (γ-1)·ρ·e |
| `idealgas_vt.F` | `/EOS/IDEAL-GAS` | Ideal gas with explicit T dependence |
| `nasg.F` | `/EOS/NASG` | Noble-Abel Stiffened Gas — dense gas / liquid |
| `noble_abel.F` | `/EOS/NOBLE-ABEL` | Noble-Abel real gas |
| `osborne.F` | `/EOS/OSBORNE` | Osborne — porous compacted materials |
| `eospolyno.F` | `/EOS/POLYNOMIAL` | Polynomial pressure-density relation |
| `eosexponential.F90` | `/EOS/EXPONENTIAL` | Exponential EOS |
| `eoslinear.F` | `/EOS/LINEAR` | Linear Us-Up Hugoniot |
| `murnaghan.F` | `/EOS/MURNAGHAN` | Murnaghan — quasi-static compression of liquids |
| `lszk.F` | `/EOS/LSZK` | Lee-Sheffields-Zerilli-Koch (propellants) |
| `stiffgas.F` | `/EOS/STIFF-GAS` | Stiffened gas (liquids at high pressure) |
| `tabulated.F` | `/EOS/TABULATED` | Tabulated P(ρ, e) lookup table |
| `powder_burn.F` | `/EOS/POWDER` | Powder burn (gun propellant) |
| `puff.F` | `/EOS/PUFF` | Puff — for vaporisation of metals |
| `sesame.F` | `/EOS/SESAME` | SESAME tabular EOS (from LANL database) |

Shared interpolation helpers for tabulated lookups:
- `mintp_rt.F`, `mintp_re.F` — interpolation in (ρ, T) and (ρ, e) tables
- `mintp1_rt.F` — 1D table interpolation
- `minter1d_rat.F` — rational 1D interpolation
- `compaction.F90`, `compaction2.F90`, `compaction_tab.F90` — compaction EOS helpers

Constructor/destructor pairs for EOS parameter structures:
- `construct_eos_param/`, `destruct_eos_param/` — allocate and free EOS parameter arrays

## Call Interface

From a material law (e.g. LAW2, LAW3, LAW4), the EOS pressure is obtained via:

```fortran
call EOSMAIN(ieos_type, rho, energy, volume, pressure, soundspeed, ...)
```

`eosmain.F` dispatches to the appropriate model based on `ieos_type`.

## Mie-Grüneisen EOS (most common for metals)

```
P = P_ref(V) + Γ(V)·ρ·(e - e_ref(V))
```

Where `P_ref` is the Hugoniot reference pressure and `Γ` is the Grüneisen parameter. The shock Hugoniot Us = C0 + s·Up is used to define `P_ref`.

## JWL (most common for explosives)

```
P = A·(1 - ω/(R1·V))·exp(-R1·V) + B·(1 - ω/(R2·V))·exp(-R2·V) + ω·e/V
```

Used for detonation product gases in explosive simulations.

## Related Documentation

- `engine/source/materials/README.md` — material laws that call EOS models
- `engine/source/ale/README.md` — ALE/Euler solvers that use EOS extensively
