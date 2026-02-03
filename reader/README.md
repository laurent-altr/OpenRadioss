# OpenRadioss Reader Library

## Overview

The OpenRadioss Reader Library (`open_reader`) is a C++ library that reads and parses OpenRadioss simulation models from input files. It stores the model data in memory and provides a clean interface for the Fortran solver code to access this data through a set of C wrapper functions.

This library enables OpenRadioss to:
- Read native Radioss `.rad` format files
- Read and convert LS-DYNA `.k`/`.key` format files (via the dyna2rad converter)
- Store model data (nodes, elements, materials, properties, etc.) in memory
- Provide standardized access to model data through the Solver Data Interface (SDI)

## Architecture

```
┌──────────────────────────────────────────────────────────────────────────────┐
│                           OpenRadioss Reader Library                          │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                               │
│  ┌─────────────────────────────────────────────────────────────────────────┐ │
│  │                        solver_interface                                  │ │
│  │  ┌───────────────┐  ┌───────────────┐  ┌────────────────────────────┐   │ │
│  │  │  C Wrapper    │  │   Model       │  │    Configuration           │   │ │
│  │  │  Functions    │  │   Access      │  │    Reading                 │   │ │
│  │  │  (Fortran ↔ C)│  │               │  │                            │   │ │
│  │  └───────────────┘  └───────────────┘  └────────────────────────────┘   │ │
│  └─────────────────────────────────────────────────────────────────────────┘ │
│                                      ↓                                        │
│  ┌─────────────────────────────────────────────────────────────────────────┐ │
│  │                              sdi (Solver Data Interface)                 │ │
│  │  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐                │ │
│  │  │  Entity       │  │  Selection    │  │  ModelView    │                │ │
│  │  │  (Read/Edit)  │  │  (Iteration)  │  │  (DB Access)  │                │ │
│  │  └───────────────┘  └───────────────┘  └───────────────┘                │ │
│  └─────────────────────────────────────────────────────────────────────────┘ │
│                                      ↓                                        │
│  ┌───────────────────────┐  ┌────────────────────┐  ┌────────────────────┐   │
│  │       cfgio           │  │     cfgkernel      │  │      dyna2rad      │   │
│  │  ┌─────────────────┐  │  │  ┌──────────────┐  │  │  ┌──────────────┐  │   │
│  │  │ Model I/O       │  │  │  │ CFG Kernel   │  │  │  │ LS-DYNA to   │  │   │
│  │  │ Factory         │  │  │  │ Descriptors  │  │  │  │ Radioss      │  │   │
│  │  │ Pre-model       │  │  │  │ Data Features│  │  │  │ Converter    │  │   │
│  │  └─────────────────┘  │  │  └──────────────┘  │  │  └──────────────┘  │   │
│  └───────────────────────┘  └────────────────────┘  └────────────────────┘   │
│                                      ↓                                        │
│  ┌─────────────────────────────────────────────────────────────────────────┐ │
│  │                        io (Model Readers)                                │ │
│  │  ┌──────────────────────────────┐  ┌────────────────────────────────┐   │ │
│  │  │    radiossblk                │  │         ls-dyna                │   │ │
│  │  │    (Radioss Block Reader)    │  │         (LS-DYNA Reader)       │   │ │
│  │  └──────────────────────────────┘  └────────────────────────────────┘   │ │
│  └─────────────────────────────────────────────────────────────────────────┘ │
│                                                                               │
└──────────────────────────────────────────────────────────────────────────────┘
```

## Module Descriptions

### 1. solver_interface

The top-level module that provides C wrapper functions for Fortran code to access model data. This is the primary interface between the C++ reader library and the Fortran solver.

**Key Directories:**
- `cfg_reading/` - Configuration reading and global model access (GlobalModelSDI)
- `elements/` - Element reading functions (shells, bricks, beams, etc.)
- `nodes/` - Node reading functions
- `includes/` - Include file handling
- `model/` - Model building and deletion
- `submodels/` - Submodel handling
- `messages/` - Message handling
- `misc/` - Miscellaneous utilities

**Key Features:**
- Provides C-callable functions with Fortran naming conventions (lowercase with underscores)
- Multiple function name variants for compatibility (e.g., `cpp_node_read_`, `CPP_NODE_READ`, `cpp_node_read`)
- Global model pointer (`g_pModelViewSDI`) for accessing the loaded model

### 2. sdi (Solver Data Interface)

The core abstraction layer that provides a unified interface for accessing solver model data. SDI defines the object model and access patterns used throughout the reader.

**Key Components:**

| Class | Purpose |
|-------|---------|
| `ModelViewRead` | Read-only access to model database |
| `ModelViewEdit` | Read/write access to model database |
| `EntityRead` / `EntityEdit` | Access to individual entities (materials, properties, etc.) |
| `SelectionRead` / `SelectionEdit` | Iterator for collections of entities |
| `HandleRead` / `HandleEdit` | Lightweight references to entities |
| `sdiValue` | Generic value container for attributes |
| `sdiTriple` | 3D coordinate container |

**Specialization Types:**
- `SPECIALIZATION_TYPE_GENERAL` - General entities (materials, properties, etc.)
- `SPECIALIZATION_TYPE_NODE` - Node entities
- `SPECIALIZATION_TYPE_ELEMENT` - Element entities

### 3. cfgio (Configuration I/O)

Handles the model I/O operations using configuration-driven parsing.

**Key Components:**
- `hw_cfg_reader` - Configuration-based reader
- `mec_pre_model` - Pre-model processing
- `mv_model_factory` - Model factory for creating model objects
- `mec_read_file` - File reading operations
- Expression evaluators for parameter handling

### 4. cfgkernel (Configuration Kernel)

Provides the kernel for interpreting configuration files that define the syntax and semantics of solver input files.

**Key Components:**
- `CFGKernel` - Main kernel class managing descriptors and subtypes
- `mv_descriptor` - Entity descriptors defining data structure
- `mv_data_*_feature` - Various data feature types (scalar, array, object, etc.)
- `mv_keywords` - Keyword management
- `mv_subtype` - Subtype definitions

### 5. dyna2rad (LS-DYNA to Radioss Converter)

Converts LS-DYNA input files to OpenRadioss format.

**Key Components:**
- `DynaToRad` - Main converter class
- `convert*` headers - Conversion routines for different entity types:
  - Nodes, Elements, Materials, Properties
  - Contacts, Constraints, Loads
  - Sets, Curves, Parameters
  - And many more...

### 6. io (Input/Output)

Contains the actual file readers for different solver formats.

**Key Components:**
- `radiossblk/` - Radioss block format reader
- `ls-dyna/` - LS-DYNA format reader
- `util/` - Utility functions

## Data Flow

### Model Reading Process

```
1. Fortran calls cpp_build_model_()
         ↓
2. cfgreader() loads the model file
         ↓
3. RadiossblkReadModelSDI() creates ModelViewEdit
         ↓
4. Model is stored in g_pModelViewSDI (global pointer)
         ↓
5. For LS-DYNA includes:
   - DynakeyReadModel() reads DYNA file
   - DynaToRad converts to Radioss format
         ↓
6. Fortran accesses data via wrapper functions:
   - cpp_node_read_()
   - cpp_shell_read_()
   - cpp_option_read_()
   - etc.
```

### Entity Access Pattern

```cpp
// Create a selection over entities of a given type
SelectionRead selection(g_pModelViewSDI, "/NODE");

// Iterate through entities
while(selection.Next())
{
    // Get entity ID
    unsigned int id = selection->GetId();
    
    // Get attribute value
    sdiValue value;
    selection->GetValue(sdiIdentifier("attributeName"), value);
    
    // Extract typed value
    double doubleValue;
    value.GetValue(doubleValue);
}
```

## Fortran-C++ Interface

The wrapper functions follow a naming convention to be callable from Fortran:

```cpp
// C++ implementation
extern "C" {
    CDECL void cpp_node_read_(int *ITAB, double *X, ...);
    
    // Alternate names for different compilers
    CDECL void CPP_NODE_READ(int *ITAB, double *X, ...);
    CDECL void cpp_node_read__(int *ITAB, double *X, ...);
    CDECL void cpp_node_read(int *ITAB, double *X, ...);
}
```

Fortran can then call:
```fortran
call cpp_node_read(ITAB, X, W, SUBID_NOD, UID_NOD)
```

## Building

The reader library is built using CMake:

```bash
cd reader
./build_script.bash -arch=linux64_gf
```

### Build Options

| Option | Description |
|--------|-------------|
| `-arch` | Target architecture (e.g., `linux64_gf`, `win64`) |
| `-com=1` | Build static library (`com_reader_${arch}`) |
| `-com=0` | Build shared library (`open_reader_${arch}`) - default |

### Output

The build produces either:
- `libopen_reader_<arch>.so` (shared library on Linux)
- `libcom_reader_<arch>.a` (static library when `-com=1`)

Libraries are copied to the `../exec` directory.

## Dependencies

- **Boost** (filesystem library) - Located in `../extlib/boost/boost_1_70_0/`
- **CMake** >= 3.15

## Key Files Reference

| File | Description |
|------|-------------|
| `source/solver_interface/source/model/cpp_build_model.cpp` | Model loading entry point |
| `source/solver_interface/source/cfg_reading/GlobalModelSdi.cpp` | Global model access |
| `source/sdi/interface/sdiModelView.h` | Main SDI interface definitions |
| `source/sdi/interface/sdiEntity.h` | Entity class definitions |
| `source/io/model_readers/radioss/radiossblk/radiossblk.h` | Radioss reader API |
| `source/dyna2rad/dyna2rad/dyna2rad.h` | DYNA to Radioss converter API |
| `source/cfgkernel/KERNEL/cfg_kernel.h` | Configuration kernel API |

## Usage Example

From Fortran, the typical usage pattern is:

```fortran
! Build the model (read input file)
call cpp_build_model(filename, size, result)

! Count nodes
call cpp_node_count(num_nodes)

! Read node data
call cpp_node_read(ITAB, X, W, SUBID_NOD, UID_NOD)

! Count elements  
call cpp_elem_count(num_shells, num_bricks, ...)

! Read element data
call cpp_shell_read(IXC, NIXC, IPARTC, ANGLE, THK, SUBID_SHELL, UID_SHELL)

! Read options/keywords
call cpp_option_start(keyword)
do while(cpp_option_next())
    call cpp_get_intv(name, value)
    call cpp_get_floatv(name, value)
end do

! Delete model when done
call cpp_delete_model()
```

## License

OpenRadioss is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0). See the LICENSE.md file in the root directory for details.
