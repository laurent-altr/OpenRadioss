# Starter AMS (Advanced Mass Scaling) Subsystem

This subsystem reads and validates the `/DT/AMS` keyword and initialises the SMS (Selective Mass Scaling) data structures.

## Key Files

| File | Role |
|------|------|
| `sms_init.F` | SMS data structure initialisation |
| `sms_auto_dt.F` | Compute the automatic target DT for SMS |

## Initialisation Steps

1. **Read `/DT/AMS`**: Get target DT, scale factor, and tolerance
2. **Identify candidate elements**: Elements whose natural DT is smaller than the target DT are candidates for mass scaling
3. **Build constraint topology**: SMS requires knowing all kinematic constraints (rigid bodies, walls, RBE2) to correctly project the scaled velocities — these are collected from the constraint subsystem
4. **Compute initial mass scaling**: `sms_auto_dt.F` calculates the minimum added mass needed to achieve the target DT, reported in the `.out` file

The resulting SMS configuration is written to the restart file for the engine to execute each step.

## Automatic DT (`sms_auto_dt.F`)

If no target DT is specified (auto mode), `sms_auto_dt.F` estimates the optimal target based on the element size distribution — choosing a DT that scales mass for only the smallest fraction of elements, minimising inertia modification while maximising time step gain.

## Related Documentation

- `engine/source/ams/README.md` — runtime SMS execution (engine)
- `engine/source/time_step/README.md` — DT control with AMS active
