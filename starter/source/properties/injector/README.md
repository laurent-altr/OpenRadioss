# Injector Property (`starter/source/properties/injector/`)

Reads injector properties for ALE fluid injection sources (`/PROP/TYPE5` and `/PROP/INJECT1`/`/INJECT2`).

## Key Files

| File | Role |
|------|------|
| `hm_read_inject1.F` | Read TYPE1 injector: mass flow rate, velocity, temperature vs. time tables |
| `hm_read_inject2.F` | Read TYPE2 injector: cone angle, spray distribution, droplet parameters |
| `hm_read_prop05.F` | Read `/PROP/TYPE5` injector property card |
| `hm_read_prop34.F` | Read `/PROP/TYPE34` injector (extended ALE injection) |

## Description

Injectors define inlet boundary conditions for ALE/Euler fluid simulations: a surface or node set through which fluid mass, momentum, and energy enter the domain at prescribed rates. `hm_read_inject1.F` handles the primary injector reading (flowrate, velocity direction, material fraction tables), while `hm_read_inject2.F` handles spray injectors with cone-angle distribution. Injector data is stored in the INIVEL/INIPRE arrays and used by the ALE solver at engine startup to initialise inlet BCs.

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `engine/source/ale/README.md` — ALE solver that uses injector BCs
