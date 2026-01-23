//Copyright>
//Copyright> Copyright (C) 1986-2026 Altair Engineering Inc.
//Copyright>
//Copyright> Permission is hereby granted, free of charge, to any person obtaining
//Copyright> a copy of this software and associated documentation files (the "Software"),
//Copyright> to deal in the Software without restriction, including without limitation
//Copyright> the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//Copyright> sell copies of the Software, and to permit persons to whom the Software is
//Copyright> furnished to do so, subject to the following conditions:
//Copyright>
//Copyright> The above copyright notice and this permission notice shall be included in all
//Copyright> copies or substantial portions of the Software.
//Copyright>
//Copyright> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//Copyright> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//Copyright> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//Copyright> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
//Copyright> WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
//Copyright> IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//Copyright>

use std::env;
use std::process;

mod reader;
mod writer;
mod types;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!(" ** ERROR: MISSING INPUT ARGUMENT: TH-FILE");
        process::exit(1);
    }

    println!("\n T01 TO CSV CONVERTER (Rust)\n");
    println!("FILE    = {}", args[1]);

    let csv_filename = if args.len() == 2 {
        format!("{}.csv", args[1])
    } else {
        format!("{}.csv", args[2])
    };

    println!("OUTPUT FILE    = {}", csv_filename);

    let t01_filename = &args[1];
    let title_filename = format!("{}_TITLES", args[1]);

    // Pre-read to get dimensions
    println!("Reading file structure...");
    let dimensions = match reader::t01_pre_read(t01_filename) {
        Ok(dims) => dims,
        Err(e) => {
            eprintln!(" ** ERROR: {}", e);
            process::exit(1);
        }
    };

    println!("  Global variables: {}", dimensions.nb_glob_var);
    println!("  Part variables: {}", dimensions.nb_part_var);
    println!("  Subset variables: {}", dimensions.nb_subs_var);
    println!("  Time steps: {}", dimensions.nb_time_step);

    // Read data
    println!("Reading data...");
    let data = match reader::t01_read(t01_filename, &dimensions) {
        Ok(d) => d,
        Err(e) => {
            eprintln!(" ** ERROR: {}", e);
            process::exit(1);
        }
    };

    // Write CSV file
    println!("Writing CSV file...");
    match writer::csv_file_write(&csv_filename, &title_filename, &data, &dimensions) {
        Ok(_) => {
            println!(" ** CONVERSION COMPLETED");
        }
        Err(e) => {
            eprintln!(" ** ERROR: {}", e);
            process::exit(1);
        }
    }
}
