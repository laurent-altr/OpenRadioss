# th_to_csv

th_to_csv is a tool to convert OpenRadioss time history files to CSV format.

This directory contains a Rust implementation of the th_to_csv converter.

## How to build

### Prerequisites

Rust toolchain installation is required. Install from https://rustup.rs/

### Building

From this directory, run:

```bash
cargo build --release
```

The executable will be located at `target/release/th_to_csv`

### Cross-compilation

For Linux x86_64:
```bash
cargo build --release --target x86_64-unknown-linux-gnu
```

For Linux ARM64:
```bash
cargo build --release --target aarch64-unknown-linux-gnu
```

For Windows:
```bash
cargo build --release --target x86_64-pc-windows-msvc
```

## How to use

Launch the converter after the simulation:

```bash
./target/release/th_to_csv [TimeHistory_File]
```

Or specify an output file:

```bash
./target/release/th_to_csv [TimeHistory_File] [OutputFile]
```

## Note

To have full variable names in .csv file, add /TH/TITLE in your .rad file when running the engine:

/TH/TITLE writes _TITLES files that contain additional information allowing full titles in the written .csv file.

OpenRadioss time history files do not contain force curves, but only impulses.
In addition to titles, /TH/TITLE writes information that allows the converter to derive impulses and write forces.

## Original C Implementation

The original C implementation has moved to [OpenRadioss/Tools/output_converters/th_to_csv](https://github.com/OpenRadioss/Tools/tree/main/output_converters/th_to_csv) repository
