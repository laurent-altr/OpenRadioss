# Nonlocal Damage Models (`starter/source/materials/nonlocal/`)

Reads `/NONLOCAL` material regularisation definitions — nonlocal averaging of damage variables to prevent mesh-dependent localization.

## Key Files

| File | Role |
|------|------|
| `hm_read_nonlocal.F` | HM binary reader for `/NONLOCAL` keyword |

## Nonlocal Regularisation

In local damage models, softening causes strain localization into a single element, making results mesh-size dependent. Nonlocal regularisation replaces the local damage variable `d` with a nonlocal average:

```
d̄(x) = ∫ w(x-y) d(y) dy  /  ∫ w(x-y) dy
```

where `w(r)` is a weight function with characteristic length `l_c`. Elements within `l_c` of each other exchange damage information, forcing the fracture zone to span multiple elements.

The starter reads the characteristic length and weight function type. The engine performs the nonlocal averaging each time step before the damage criterion is evaluated.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/materials/README.md` — nonlocal damage computation in engine
