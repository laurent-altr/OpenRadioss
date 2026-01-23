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

pub type ConversionResult<T> = Result<T, Box<dyn std::error::Error>>;

/// Dimensions of the T01 file
#[derive(Debug, Clone)]
pub struct Dimensions {
    pub nb_glob_var: usize,
    pub nb_part_var: usize,
    pub nb_subs_var: usize,
    pub nb_time_step: usize,
    pub cpt_data: usize,
    pub cpt_thgroup_names: usize,
}

/// T01 data structure containing all parsed data
#[derive(Debug)]
pub struct T01Data {
    pub all_data: Vec<f32>,
    pub th_part_names: Vec<String>,
    pub th_subs_names: Vec<String>,
    pub th_group_names: Vec<String>,
}

/// Variable type codes from T01 format
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarType {
    InternalEnergy = 1,
    KineticEnergy = 2,
    Xmomentum = 3,
    Ymomentum = 4,
    Zmomentum = 5,
    Xvelocity = 6,
    Yvelocity = 7,
    Zvelocity = 8,
    Xforce = 9,
    Yforce = 10,
    Zforce = 11,
    Mass = 12,
}

impl VarType {
    pub fn from_code(code: i32) -> Option<Self> {
        match code {
            1 => Some(VarType::InternalEnergy),
            2 => Some(VarType::KineticEnergy),
            3 => Some(VarType::Xmomentum),
            4 => Some(VarType::Ymomentum),
            5 => Some(VarType::Zmomentum),
            6 => Some(VarType::Xvelocity),
            7 => Some(VarType::Yvelocity),
            8 => Some(VarType::Zvelocity),
            9 => Some(VarType::Xforce),
            10 => Some(VarType::Yforce),
            11 => Some(VarType::Zforce),
            12 => Some(VarType::Mass),
            _ => None,
        }
    }

    pub fn to_suffix(&self) -> String {
        match self {
            VarType::InternalEnergy => "IE".to_string(),
            VarType::KineticEnergy => "KE".to_string(),
            VarType::Xmomentum => "XMOM".to_string(),
            VarType::Ymomentum => "YMOM".to_string(),
            VarType::Zmomentum => "ZMOM".to_string(),
            VarType::Xvelocity => "XVEL".to_string(),
            VarType::Yvelocity => "YVEL".to_string(),
            VarType::Zvelocity => "ZVEL".to_string(),
            VarType::Xforce => "XF".to_string(),
            VarType::Yforce => "YF".to_string(),
            VarType::Zforce => "ZF".to_string(),
            VarType::Mass => "MASS".to_string(),
        }
    }
}
