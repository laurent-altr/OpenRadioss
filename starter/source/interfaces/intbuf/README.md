# Interface Buffer (`starter/source/interfaces/intbuf/`)

Defines and initialises the INTBUF contact buffer data structure used by all contact types to store candidate node-segment pairs.

## Key Files

| File | Role |
|------|------|
| `intbuf_ini_starter.F` | Initialise the INTBUF candidate-pair buffer arrays |
| `intbufFric_ini_starter.F` | Initialise the friction state buffer (tangential displacement, sliding history) |

## Description

INTBUF is the flat array that stores, for each candidate contact pair: slave node ID, master segment ID, gap, normal direction, and penalty force. `intbuf_ini_starter.F` allocates and zeros this buffer based on the expected number of contact candidates (estimated from search radius and mesh density). `intbufFric_ini_starter.F` additionally allocates the friction tangential-displacement history arrays needed for incremental Coulomb friction.

## Related Documentation

- `starter/source/interfaces/README.md` — parent directory
- `common_source/modules/interfaces/README.md` — INTBUF module definition
- `engine/source/interfaces/README.md` — runtime contact enforcement using INTBUF
