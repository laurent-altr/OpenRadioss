# Starter Laser Load (`starter/source/loads/laser/`)

Reads laser heat flux load definition (`/LOAD/LASER`).

## Key Files

| File | Role |
|------|------|
| `leclas.F` | Main laser input reader |
| `leclas1.F` | Laser TYPE1 parameters: Gaussian spot, power, spot radius |
| `lpreleclas.F` | Pre-read pass for laser array sizing |
| `laser10.F` | Laser model 10 parameters |
| `laser20.F` | Laser model 20 parameters |
| `laserp.F` | Laser power/scan-path parameters |

## Related Documentation

- `starter/source/loads/README.md` — parent loads directory
- `engine/source/loads/laser/README.md` — engine laser application
