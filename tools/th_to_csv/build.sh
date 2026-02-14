#!/bin/bash
#Copyright>
#Copyright> Copyright (C) 1986-2026 Altair Engineering Inc.
#Copyright>
#Copyright> Permission is hereby granted, free of charge, to any person obtaining
#Copyright> a copy of this software and associated documentation files (the "Software"),
#Copyright> to deal in the Software without restriction, including without limitation
#Copyright> the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
#Copyright> sell copies of the Software, and to permit persons to whom the Software is
#Copyright> furnished to do so, subject to the following conditions:
#Copyright>
#Copyright> The above copyright notice and this permission notice shall be included in all
#Copyright> copies or substantial portions of the Software.
#Copyright>
#Copyright> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#Copyright> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#Copyright> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#Copyright> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#Copyright> WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
#Copyright> IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#Copyright>

# Build script for th_to_csv Rust version

echo "Building th_to_csv (Rust)..."

# Check if cargo is installed
if ! command -v cargo &> /dev/null
then
    echo "Error: Cargo (Rust toolchain) is not installed."
    echo "Please install Rust from https://rustup.rs/"
    exit 1
fi

# Build release version
cargo build --release

if [ $? -eq 0 ]; then
    echo "Build successful!"
    echo "Executable: target/release/th_to_csv"
    
    # Optionally copy to exec directory if it exists
    if [ -d "../../exec" ]; then
        cp target/release/th_to_csv ../../exec/th_to_csv.rust
        echo "Copied to ../../exec/th_to_csv.rust"
    fi
else
    echo "Build failed!"
    exit 1
fi
