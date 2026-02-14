# th_to_csv - Rust Implementation

## Overview

This is a Rust implementation of the OpenRadioss time history file to CSV converter. It reads binary T01 files produced by OpenRadioss simulations and converts them to comma-separated values (CSV) format for easy analysis in spreadsheet applications or data analysis tools.

## Features

- **Fast binary reading**: Efficient parsing of Fortran-formatted binary files
- **Memory-safe**: Rust's memory safety guarantees prevent common bugs
- **Cross-platform**: Compiles for Linux, Windows, and macOS
- **Full variable support**: Handles global variables, part variables, subsets, and element groups
- **Impulse-to-force conversion**: Automatically derives forces from impulses when _TITLES file is present

## Building

### Prerequisites

Install the Rust toolchain from [rustup.rs](https://rustup.rs/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Build Commands

**Quick build:**
```bash
./build.sh
```

**Manual build:**
```bash
# Debug build (faster compilation, slower execution)
cargo build

# Release build (slower compilation, optimized execution)
cargo build --release
```

The executable will be at:
- Debug: `target/debug/th_to_csv`
- Release: `target/release/th_to_csv`

## Usage

### Basic Conversion

Convert a T01 file to CSV:
```bash
./target/release/th_to_csv path/to/simulation.T01
```

This creates `path/to/simulation.T01.csv`

### Custom Output Name

Specify a custom output filename:
```bash
./target/release/th_to_csv path/to/simulation.T01 output_data
```

This creates `output_data.csv`

### With Force Conversion

If you have a `_TITLES` file (generated with `/TH/TITLE` in your .rad file):
```bash
# The converter automatically looks for simulation.T01_TITLES
./target/release/th_to_csv path/to/simulation.T01
```

## File Format Details

### Input: T01 Binary Format

The T01 file is a Fortran binary file with the following structure:
- **Header**: Version, title, scaling factors
- **Hierarchy**: Part names, material names, geometry names
- **Variables**: Variable type definitions (energy, momentum, velocity, force, etc.)
- **Data**: Time-series data for each variable at each timestep

### Output: CSV Format

The CSV file has:
- **Header row**: Column names (Time, GLOB_1, GLOB_2, ..., Part_1 IE, Part_1 KE, ...)
- **Data rows**: One row per timestep with values for all variables

Example:
```csv
Time,GLOB_1,GLOB_2,Part_1 IE,Part_1 KE,Part_1 XMOM
0.0,100.5,250.3,50.2,50.3,0.0
0.001,101.2,251.0,50.5,50.7,1.2
0.002,102.0,252.1,51.0,51.0,2.5
```

### Variable Type Suffixes

- **IE**: Internal Energy
- **KE**: Kinetic Energy
- **XMOM/YMOM/ZMOM**: Momentum (X/Y/Z direction)
- **XVEL/YVEL/ZVEL**: Velocity (X/Y/Z direction)
- **XF/YF/ZF**: Force (X/Y/Z direction)
- **MASS**: Mass

## Impulse-to-Force Conversion

OpenRadioss time history files store impulses rather than forces. When a `_TITLES` file is present, the converter can identify impulse variables and calculate forces using finite differences:

- **Interior points**: Central difference: `F(t) = (I(t+Δt) - I(t-Δt)) / (2Δt)`
- **First point**: Forward difference: `F(t₀) = (I(t₁) - I(t₀)) / Δt`
- **Last point**: Backward difference: `F(tₙ) = (I(tₙ) - I(tₙ₋₁)) / Δt`

## Performance

The Rust implementation offers:
- **Speed**: Comparable or faster than the C version
- **Memory efficiency**: Minimal allocations, streaming where possible
- **Safety**: No buffer overflows or memory leaks

Typical performance: ~100-500 MB/s file reading speed, depending on hardware.

## Troubleshooting

### "ERROR: FILE not found"
- Check that the T01 file path is correct
- Ensure you have read permissions

### "ERROR: Unexpected end of file"
- The T01 file may be corrupted
- Check if the simulation completed successfully

### Missing variable names
- Run OpenRadioss with `/TH/TITLE` option to generate `_TITLES` file
- This provides full variable names and enables force conversion

## Comparison with C Version

| Feature | C Version | Rust Version |
|---------|-----------|--------------|
| Speed | Fast | Fast |
| Memory Safety | Manual | Automatic |
| Cross-platform | Manual makefiles | Cargo handles it |
| Dependencies | GCC/MSVC | Rust toolchain |
| Binary size | ~100-200KB | ~400-500KB |

## Contributing

This is part of the OpenRadioss project. Contributions welcome!

## License

MIT License - see LICENSE.md in the root directory
