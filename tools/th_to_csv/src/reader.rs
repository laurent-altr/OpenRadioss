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
use std::io::{BufReader, Read};

use crate::types::{ConversionResult, Dimensions, T01Data, VarType};

/// Binary reader for Fortran-style records
struct FortranReader {
    reader: BufReader<File>,
}

impl FortranReader {
    fn new(file: File) -> Self {
        FortranReader {
            reader: BufReader::new(file),
        }
    }

    /// Read end-of-record marker (4 bytes)
    fn read_eor(&mut self) -> ConversionResult<i32> {
        let mut buf = [0u8; 4];
        self.reader.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    /// Read integers
    fn read_integers(&mut self, count: usize) -> ConversionResult<Vec<i32>> {
        let mut result = Vec::with_capacity(count);
        let mut buf = [0u8; 4];
        for _ in 0..count {
            self.reader.read_exact(&mut buf)?;
            result.push(i32::from_le_bytes(buf));
        }
        Ok(result)
    }

    /// Read a single integer
    fn read_integer(&mut self) -> ConversionResult<i32> {
        let mut buf = [0u8; 4];
        self.reader.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    /// Read floats
    fn read_floats(&mut self, count: usize) -> ConversionResult<Vec<f32>> {
        let mut result = Vec::with_capacity(count);
        let mut buf = [0u8; 4];
        for _ in 0..count {
            self.reader.read_exact(&mut buf)?;
            result.push(f32::from_le_bytes(buf));
        }
        Ok(result)
    }

    /// Read a single float
    fn read_float(&mut self) -> ConversionResult<f32> {
        let mut buf = [0u8; 4];
        self.reader.read_exact(&mut buf)?;
        Ok(f32::from_le_bytes(buf))
    }

    /// Read characters (as bytes)
    fn read_chars(&mut self, count: usize) -> ConversionResult<Vec<u8>> {
        let mut buf = vec![0u8; count];
        self.reader.read_exact(&mut buf)?;
        Ok(buf)
    }

    /// Convert character buffer to string, trimming nulls and whitespace
    fn chars_to_string(chars: &[u8]) -> String {
        String::from_utf8_lossy(chars)
            .trim_end_matches('\0')
            .trim()
            .to_string()
    }
}

/// Pre-read the T01 file to determine dimensions
pub fn t01_pre_read(filename: &str) -> ConversionResult<Dimensions> {
    let file = File::open(filename)?;
    let mut reader = FortranReader::new(file);

    // Read TITRE record
    reader.read_eor()?;
    let thicode = reader.read_integer()?;
    
    let title_length = if thicode >= 4021 {
        100
    } else if thicode >= 3041 {
        80
    } else {
        40
    };

    reader.read_chars(80)?; // Read title
    reader.read_eor()?;

    // Read version/date record
    reader.read_eor()?;
    reader.read_chars(80)?;
    reader.read_eor()?;

    // Read additional records if version > 3050
    if thicode > 3050 {
        reader.read_eor()?;
        reader.read_integer()?; // Some integer
        reader.read_eor()?;

        reader.read_eor()?;
        reader.read_integer()?; // Title length
        reader.read_eor()?;

        reader.read_eor()?;
        reader.read_float()?; // FAC_MASS
        reader.read_float()?; // FAC_LENGTH
        reader.read_float()?; // FAC_TIME
        reader.read_eor()?;
    }

    // Read hierarchy info
    reader.read_eor()?;
    let _npart_nthpart = reader.read_integer()? as usize;
    let _nummat = reader.read_integer()?;
    let _numgeo = reader.read_integer()?;
    let _nsubs = reader.read_integer()? as usize;
    let _nthgrp2 = reader.read_integer()? as usize;
    let _nglob = reader.read_integer()? as usize;
    reader.read_eor()?;

    // Skip to hierarchy info again
    reader.read_eor()?;
    reader.read_integer()?; // thicode
    reader.read_chars(80)?;
    reader.read_eor()?;

    reader.read_eor()?;
    reader.read_chars(80)?;
    reader.read_eor()?;

    if thicode > 3050 {
        reader.read_eor()?;
        reader.read_integer()?;
        reader.read_eor()?;

        reader.read_eor()?;
        reader.read_integer()?;
        reader.read_eor()?;

        reader.read_eor()?;
        reader.read_float()?;
        reader.read_float()?;
        reader.read_float()?;
        reader.read_eor()?;
    }

    reader.read_eor()?;
    let npart_nthpart = reader.read_integer()? as usize;
    let _nummat = reader.read_integer()?;
    let _numgeo = reader.read_integer()?;
    let nsubs = reader.read_integer()? as usize;
    let nthgrp2 = reader.read_integer()? as usize;
    let nglob = reader.read_integer()? as usize;
    reader.read_eor()?;

    let mut nvar_part_tot = 0;
    let mut nvar_subs = 0;
    let mut cpt_thgroup_names = 0;

    // Read part descriptions
    for _ in 0..npart_nthpart {
        reader.read_eor()?;
        reader.read_integer()?; // ID
        reader.read_chars(title_length)?; // Name
        let nvar = reader.read_integer()? as usize;
        nvar_part_tot += nvar;
        reader.read_integers(nvar)?; // Variable types
        reader.read_eor()?;
    }

    // Read material descriptions
    for _ in 0..npart_nthpart {
        reader.read_eor()?;
        reader.read_integer()?; // NUMMAT
        reader.read_chars(title_length)?; // Name
        reader.read_eor()?;
    }

    // Read geometry descriptions
    for _ in 0..npart_nthpart {
        reader.read_eor()?;
        reader.read_integer()?; // NUMGEO
        reader.read_chars(title_length)?; // Name
        reader.read_eor()?;
    }

    // Read subset descriptions
    for _ in 0..nsubs {
        reader.read_eor()?;
        reader.read_integer()?; // ID
        reader.read_chars(title_length)?; // Name
        let nbpartf = reader.read_integer()? as usize;
        let nbsubsf = reader.read_integer()? as usize;
        let nvar = reader.read_integer()? as usize;
        nvar_subs += nvar;
        
        reader.read_integers(nbpartf)?; // Part IDs
        reader.read_integers(nbsubsf)?; // Subset IDs
        reader.read_integers(nvar)?; // Variable types
        reader.read_eor()?;
    }

    // Read time history groups
    for _ in 0..nthgrp2 {
        reader.read_eor()?;
        let nbelem = reader.read_integer()? as usize;
        reader.read_chars(title_length)?; // Name
        let nvar = reader.read_integer()? as usize;
        
        reader.read_integers(nbelem)?; // Element IDs
        
        for _ in 0..nvar {
            reader.read_chars(title_length)?; // Variable name
            cpt_thgroup_names += 1;
        }
        reader.read_eor()?;
    }

    // Count timesteps by reading data
    let mut nb_time_step = 0;
    loop {
        // Try to read time record
        match reader.read_eor() {
            Ok(_) => {},
            Err(_) => break,
        }
        
        match reader.read_float() {
            Ok(_) => {},
            Err(_) => break,
        }
        
        match reader.read_eor() {
            Ok(_) => {},
            Err(_) => break,
        }

        nb_time_step += 1;

        // Skip global variables
        if nglob > 0 {
            reader.read_eor()?;
            reader.read_floats(nglob)?;
            reader.read_eor()?;
        }

        // Skip part variables
        if nvar_part_tot > 0 {
            reader.read_eor()?;
            reader.read_floats(nvar_part_tot)?;
            reader.read_eor()?;
        }

        // Skip subset variables
        if nvar_subs > 0 {
            reader.read_eor()?;
            reader.read_floats(nvar_subs)?;
            reader.read_eor()?;
        }

        // Skip element group data
        for _ in 0..cpt_thgroup_names {
            reader.read_eor()?;
            reader.read_float()?;
            reader.read_eor()?;
        }
    }

    let total_vars = 1 + nglob + nvar_part_tot + nvar_subs + cpt_thgroup_names; // +1 for time
    let cpt_data = nb_time_step * total_vars;

    Ok(Dimensions {
        nb_glob_var: nglob,
        nb_part_var: nvar_part_tot,
        nb_subs_var: nvar_subs,
        nb_time_step,
        cpt_data,
        cpt_thgroup_names,
    })
}

/// Read the T01 file and extract all data
pub fn t01_read(filename: &str, dimensions: &Dimensions) -> ConversionResult<T01Data> {
    let file = File::open(filename)?;
    let mut reader = FortranReader::new(file);

    // Read TITRE record
    reader.read_eor()?;
    let thicode = reader.read_integer()?;
    
    let title_length = if thicode >= 4021 {
        100
    } else if thicode >= 3041 {
        80
    } else {
        40
    };

    reader.read_chars(80)?;
    reader.read_eor()?;

    // Read version/date record
    reader.read_eor()?;
    reader.read_chars(80)?;
    reader.read_eor()?;

    // Read additional records if version > 3050
    if thicode > 3050 {
        reader.read_eor()?;
        reader.read_integer()?;
        reader.read_eor()?;

        reader.read_eor()?;
        reader.read_integer()?;
        reader.read_eor()?;

        reader.read_eor()?;
        reader.read_float()?;
        reader.read_float()?;
        reader.read_float()?;
        reader.read_eor()?;
    }

    // Read hierarchy info
    reader.read_eor()?;
    let npart_nthpart = reader.read_integer()? as usize;
    let _nummat = reader.read_integer()?;
    let _numgeo = reader.read_integer()?;
    let nsubs = reader.read_integer()? as usize;
    let nthgrp2 = reader.read_integer()? as usize;
    let nglob = reader.read_integer()? as usize;
    reader.read_eor()?;

    // Read part names and variable types
    let mut th_part_names = Vec::new();
    let mut part_ids = Vec::new();
    
    for _ in 0..npart_nthpart {
        reader.read_eor()?;
        let id = reader.read_integer()?;
        let name_bytes = reader.read_chars(title_length)?;
        let name = FortranReader::chars_to_string(&name_bytes);
        let nvar = reader.read_integer()? as usize;
        let var_types = reader.read_integers(nvar)?;
        
        part_ids.push(id);
        
        for var_type in var_types {
            let suffix = VarType::from_code(var_type)
                .map(|vt| vt.to_suffix())
                .unwrap_or_else(|| "UNKNOWN".to_string());
            th_part_names.push(format!("{} {}", name, suffix));
        }
        
        reader.read_eor()?;
    }

    // Read material names (skip)
    for _ in 0..npart_nthpart {
        reader.read_eor()?;
        reader.read_integer()?;
        reader.read_chars(title_length)?;
        reader.read_eor()?;
    }

    // Read geometry names (skip)
    for _ in 0..npart_nthpart {
        reader.read_eor()?;
        reader.read_integer()?;
        reader.read_chars(title_length)?;
        reader.read_eor()?;
    }

    // Read subset names and variable types
    let mut th_subs_names = Vec::new();
    
    for _ in 0..nsubs {
        reader.read_eor()?;
        let _id = reader.read_integer()?;
        let name_bytes = reader.read_chars(title_length)?;
        let name = FortranReader::chars_to_string(&name_bytes);
        let nbpartf = reader.read_integer()? as usize;
        let nbsubsf = reader.read_integer()? as usize;
        let nvar = reader.read_integer()? as usize;
        
        reader.read_integers(nbpartf)?;
        reader.read_integers(nbsubsf)?;
        let var_types = reader.read_integers(nvar)?;
        
        for var_type in var_types {
            let suffix = VarType::from_code(var_type)
                .map(|vt| vt.to_suffix())
                .unwrap_or_else(|| "UNKNOWN".to_string());
            th_subs_names.push(format!("{} {}", name, suffix));
        }
        
        reader.read_eor()?;
    }

    // Read time history group names
    let mut th_group_names = Vec::new();
    
    for _ in 0..nthgrp2 {
        reader.read_eor()?;
        let nbelem = reader.read_integer()? as usize;
        reader.read_chars(title_length)?; // Group name
        let nvar = reader.read_integer()? as usize;
        
        reader.read_integers(nbelem)?;
        
        for _ in 0..nvar {
            let var_name_bytes = reader.read_chars(title_length)?;
            let var_name = FortranReader::chars_to_string(&var_name_bytes);
            th_group_names.push(var_name);
        }
        
        reader.read_eor()?;
    }

    // Read all timestep data
    let _total_vars = 1 + nglob + dimensions.nb_part_var + dimensions.nb_subs_var + dimensions.cpt_thgroup_names;
    let mut all_data = Vec::with_capacity(dimensions.cpt_data);

    for _ in 0..dimensions.nb_time_step {
        // Read time
        reader.read_eor()?;
        let time = reader.read_float()?;
        reader.read_eor()?;
        all_data.push(time);

        // Read global variables
        if nglob > 0 {
            reader.read_eor()?;
            let glob_data = reader.read_floats(nglob)?;
            all_data.extend(glob_data);
            reader.read_eor()?;
        }

        // Read part variables
        if dimensions.nb_part_var > 0 {
            reader.read_eor()?;
            let part_data = reader.read_floats(dimensions.nb_part_var)?;
            all_data.extend(part_data);
            reader.read_eor()?;
        }

        // Read subset variables
        if dimensions.nb_subs_var > 0 {
            reader.read_eor()?;
            let subs_data = reader.read_floats(dimensions.nb_subs_var)?;
            all_data.extend(subs_data);
            reader.read_eor()?;
        }

        // Read element group data
        for _ in 0..dimensions.cpt_thgroup_names {
            reader.read_eor()?;
            let group_val = reader.read_float()?;
            all_data.push(group_val);
            reader.read_eor()?;
        }
    }

    Ok(T01Data {
        all_data,
        th_part_names,
        th_subs_names,
        th_group_names,
    })
}
