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

use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;

use crate::types::{ConversionResult, Dimensions, T01Data};

/// Parse the _TITLES file to identify impulse variables
fn parse_titles_file(filename: &str) -> ConversionResult<Vec<bool>> {
    let mut is_impulse = Vec::new();
    
    if !Path::new(filename).exists() {
        return Ok(is_impulse);
    }

    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line?;
        let trimmed = line.trim();
        
        // Check if line indicates impulse variable
        // The _TITLES file format marks impulse variables with special flags
        if trimmed.contains("IMPULSE") || trimmed.contains("impulse") {
            is_impulse.push(true);
        } else if !trimmed.is_empty() && !trimmed.starts_with('#') {
            is_impulse.push(false);
        }
    }

    Ok(is_impulse)
}

/// Write CSV file from T01 data
pub fn csv_file_write(
    csv_filename: &str,
    title_filename: &str,
    data: &T01Data,
    dimensions: &Dimensions,
) -> ConversionResult<()> {
    let mut file = File::create(csv_filename)?;

    let total_vars = 1 + dimensions.nb_glob_var + dimensions.nb_part_var + 
                     dimensions.nb_subs_var + dimensions.cpt_thgroup_names;

    // Try to parse _TITLES file to identify impulse variables
    let is_impulse = parse_titles_file(title_filename).unwrap_or_default();
    
    // Build column headers
    let mut headers = vec!["Time".to_string()];
    
    // Add global variable headers
    for i in 0..dimensions.nb_glob_var {
        headers.push(format!("GLOB_{}", i + 1));
    }
    
    // Add part variable headers
    for name in &data.th_part_names {
        headers.push(name.clone());
    }
    
    // Add subset variable headers
    for name in &data.th_subs_names {
        headers.push(name.clone());
    }
    
    // Add group variable headers
    for name in &data.th_group_names {
        headers.push(name.clone());
    }

    // Write header line
    writeln!(file, "{}", headers.join(","))?;

    // Extract time column
    let mut times = Vec::new();
    for timestep in 0..dimensions.nb_time_step {
        let time_idx = timestep * total_vars;
        times.push(data.all_data[time_idx]);
    }

    // Write data rows with impulse-to-force conversion where needed
    for timestep in 0..dimensions.nb_time_step {
        let row_start = timestep * total_vars;
        
        // Write time
        write!(file, "{}", data.all_data[row_start])?;
        
        // Write other variables
        for var_idx in 1..total_vars {
            let data_idx = row_start + var_idx;
            let value = data.all_data[data_idx];
            
            // Check if this variable needs impulse-to-force conversion
            let needs_conversion = var_idx < is_impulse.len() && is_impulse[var_idx];
            
            if needs_conversion && dimensions.nb_time_step > 1 {
                // Calculate force from impulse using central difference
                let force = if timestep == 0 {
                    // Forward difference for first timestep
                    let next_idx = (timestep + 1) * total_vars + var_idx;
                    let next_value = data.all_data[next_idx];
                    let dt = times[timestep + 1] - times[timestep];
                    if dt != 0.0 {
                        (next_value - value) / dt
                    } else {
                        0.0
                    }
                } else if timestep == dimensions.nb_time_step - 1 {
                    // Backward difference for last timestep
                    let prev_idx = (timestep - 1) * total_vars + var_idx;
                    let prev_value = data.all_data[prev_idx];
                    let dt = times[timestep] - times[timestep - 1];
                    if dt != 0.0 {
                        (value - prev_value) / dt
                    } else {
                        0.0
                    }
                } else {
                    // Central difference for interior points
                    let prev_idx = (timestep - 1) * total_vars + var_idx;
                    let next_idx = (timestep + 1) * total_vars + var_idx;
                    let prev_value = data.all_data[prev_idx];
                    let next_value = data.all_data[next_idx];
                    let dt = times[timestep + 1] - times[timestep - 1];
                    if dt != 0.0 {
                        (next_value - prev_value) / dt
                    } else {
                        0.0
                    }
                };
                write!(file, ",{}", force)?;
            } else {
                write!(file, ",{}", value)?;
            }
        }
        
        writeln!(file)?;
    }

    Ok(())
}
