# Gauge (Output Point) Definitions (`starter/source/output/gauge/`)

Reads gauge (strain gauge / stress gauge) output point definitions.

## Key Files

| File | Role |
|------|------|
| `hm_read_gauge.F` | HM binary reader for `/GAUGE` definitions |

## Gauges

A gauge (`/GAUGE`) is a virtual measurement point attached to a node or element surface that records:
- Local strains and stresses at the gauge location
- Transformed to the gauge's local orientation frame
- Output at TH (time history) frequency

Gauges are commonly used to compare simulation results with physical strain gauge measurements in test correlation.

The starter reads the gauge location, orientation, and output frame, and writes these to the restart file. The engine evaluates gauge quantities each output step.

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `starter/source/output/th/README.md` — TH output drives gauge recording frequency
