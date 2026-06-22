# User-Defined Windows (`starter/source/tools/userwi/`)

Reads `/WINDOW/USER` (user-defined window) definitions for time-windowed output filtering.

## Key Files

| File | Role |
|------|------|
| `hm_read_window_user.F` | HM binary reader for `/WINDOW/USER` keyword |

## User Windows

A user-defined window (`/WINDOW/USER`) specifies a time interval during which a sensor or output quantity is evaluated. This is used in injury criteria calculations (HIC, NIC) where the criterion value is the maximum over a sliding time window of specified duration.

The starter reads the window parameters (duration, step size) and writes them to the restart file. The engine applies the window during sensor evaluation.

## Related Documentation

- `starter/source/tools/README.md` — parent tools directory
- `starter/source/tools/sensor/README.md` — sensor definitions that use windows (HIC, NIC)
