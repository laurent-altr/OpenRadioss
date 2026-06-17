# Accelerometer Definitions (`starter/source/tools/accele/`)

Reads `/ACCEL` (accelerometer) definitions used for nodal acceleration output and sensor triggering.

## Key Files

| File | Role |
|------|------|
| `lecacc.F` | Read `/ACCEL` keyword: node ID, optional local frame, output filter settings |

## What is an Accelerometer?

An `/ACCEL` accelerometer is a virtual gauge attached to a node that records the acceleration time history. It supports:
- Output in global or local (skew) frame
- Optional low-pass filtering (SAE/Butterworth) applied to the time history
- Use as a `/SENSOR/ACCEL` trigger source

The starter reads the accelerometer definition (node reference, frame, filter parameters) and writes it to the restart file. The engine evaluates acceleration at each output step.

## Related Documentation

- `starter/source/tools/README.md` — parent tools directory
- `starter/source/tools/sensor/README.md` — `/SENSOR/ACCEL` uses accelerometer data
