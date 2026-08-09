# Porous Media (`engine/source/ale/porous/`)

Models flow through porous media in ALE/Euler simulations (Darcy flow and momentum sinks).

## Key Files

| File | Role |
|------|------|
| `poro.F` | Main porous flow computation: compute Darcy drag force on fluid passing through porous element |

## Porous Media Model

Porous media (e.g., foam padding, fibrous absorbers, perforated panels) in an ALE mesh is modelled via Darcy's law:

```
∇p = -μ/K × u - C_F/√K × ρ|u|u
```

where:
- `K` = permeability [m²]
- `C_F` = Forchheimer coefficient (nonlinear drag)
- `μ` = dynamic viscosity
- `u` = fluid velocity

This adds a body force (drag) to the fluid momentum equation for elements flagged as porous. Used for:
- Airbag internal venting through fabric porosity
- Acoustic absorber modelling
- Filtration and foam modelling

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/airbag/README.md` — airbag porosity uses similar models
