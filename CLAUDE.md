# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is OpenRadioss

OpenRadioss is an open-source finite element analysis solver for highly nonlinear problems under dynamic loading (crash, impact, blast). It is composed of two sequential binaries:

1. **Starter** (`starter/`) — reads and validates the model, splits the mesh for parallel execution
2. **Engine** (`engine/`) — runs the actual simulation (explicit time integration, MPI+OpenMP parallelism)

Binaries are placed in `exec/` after a successful build.

## Building

Both the starter and engine have their own `build_script.sh` (Linux) or `build_windows.bat` (Windows). Run these from within their respective subdirectories.

### Linux (GFortran, recommended for development)

```bash
# Build starter
cd starter
./build_script.sh -arch=linux64_gf -release

# Build engine (SMP only)
cd engine
./build_script.sh -arch=linux64_gf -release

# Build engine with OpenMPI
cd engine
./build_script.sh -arch=linux64_gf -mpi=ompi -release
```

**For developers, build with AddressSanitizer instead of `-release`:**

```bash
./build_script.sh -arch=linux64_gf -debug=asan
```

### Key build flags

| Flag | Description |
|------|-------------|
| `-arch=linux64_gf` | Linux x86-64 with GFortran (default for open-source) |
| `-arch=linux64_ifx` | Linux x86-64 with Intel OneAPI ifx |
| `-arch=linuxa64` | Linux ARM64 with ArmFlang |
| `-arch=win64` | Windows with Intel OneAPI |
| `-mpi=ompi` | Build with OpenMPI support |
| `-prec=dp` | Double precision (default) |
| `-prec=sp` | Single/extended precision |
| `-debug=asan` | Address sanitizer (Linux/GFortran, recommended for dev) |
| `-debug=chkb` | Check bounds (Windows, recommended for dev) |
| `-static-link` | Statically link Fortran/C runtimes |
| `-nt=N` | Use N threads to speed up build |
| `-clean` | Clean build directory |

Third-party libraries are fetched automatically from the `OpenRadioss_extlib` repository during build if not already present.

## Running

Set environment variables before running:

```bash
export OPENRADIOSS_PATH=/path/to/OpenRadioss
export RAD_CFG_PATH=$OPENRADIOSS_PATH/hm_cfg_files
export RAD_H3D_PATH=$OPENRADIOSS_PATH/extlib/h3d/lib/linux64
export OMP_STACKSIZE=400m
export LD_LIBRARY_PATH=$OPENRADIOSS_PATH/extlib/hm_reader/linux64/:$LD_LIBRARY_PATH
```

```bash
# Run (SMP, 4 threads)
export OMP_NUM_THREADS=4
./exec/starter_linux64_gf -i model_0000.rad -np 1
./exec/engine_linux64_gf  -i model_0001.rad

# Run with MPI (2 processes x 2 threads)
export OMP_NUM_THREADS=2
./exec/starter_linux64_gf -i model_0000.rad -np 2
mpiexec -n 2 --map-by socket:PE=2 --bind-to core ./exec/engine_linux64_gf_ompi -i model_0001.rad
```

Input formats: `.rad` (native Radioss), `.k`/`.key` (LS-Dyna), `.inp` (Abaqus via converter).

## Running the QA Test Suite

Tests are in `qa-tests/miniqa/` organized by physics category (CRASH, ALE2D, AIRBAG, EOS, etc.).

```bash
cd qa-tests/scripts

# Without MPI
perl ./or_qa_script ../../exec/engine_linux64_gf 1.0

# With MPI (4 processes, 2 threads each)
export OMP_NUM_THREADS=2
perl ./or_qa_script ../../exec/engine_linux64_gf_ompi 1.0 \
  --env:RAD_CFG_PATH=../../hm_cfg_files \
  --env:OMP_STACKSIZE=400m \
  --exec_script_args='mpiexec -np 4'
```

The `1.0` argument is a tolerance multiplier for result comparisons. Results are summarized in `QA.summary`.

### Running a single test

Each test case in `qa-tests/miniqa/<CATEGORY>/<testname>/` contains the input `.rad` files and reference `.extract` output. To run a single test manually, run starter then engine from the test directory, then compare outputs.

## Code Architecture

### Source organization

```
common_source/      # Shared Fortran modules and include files used by both starter and engine
  modules/          # Fortran modules (precision_mod.F90, constant_mod.F, etc.)
  includes/         # Legacy .inc header files
starter/source/     # Starter-specific physics: mesh reading, domain decomposition
engine/source/      # Engine physics: time integration, elements, materials, contact
reader/             # Open_Reader: C++ library for reading .k/.dyn LS-Dyna input files
hm_cfg_files/       # Configuration files describing input deck syntax (parsed at runtime)
```

### Starter vs Engine split

The **Starter** reads the full model, performs consistency checks, and writes domain-decomposed restart files (`_NNNN.rst`). The **Engine** picks up these restart files and runs the explicit time-stepping loop. They share modules from `common_source/` but have independent `source/` trees.

### Shared module conventions

- `precision_mod.F90` — defines `WP` (working precision kind, either 4 or 8 depending on `-prec`)
- `constant_mod.F` — physical constants (PI, etc.)
- `mvsiz_mod` — defines `MVSIZ`, the preferred vector-loop chunk size for performance
- `spmd_mod.F90` — MPI wrapper (`SPMD_*` functions mirror `MPI_*` with Fortran-friendly interfaces; always use this instead of calling MPI directly)
- `my_alloc_mod` — safe allocate/deallocate wrapper with error checking

### Parallelism model

OpenRadioss uses a hybrid MPI + OpenMP model:
- MPI decomposes the mesh spatially (done by Starter `-np N`)
- OpenMP parallelizes loops within each MPI domain
- The `MVSIZ` chunk size is the canonical inner loop length for vectorization

## Fortran Coding Standards

New code must be `.F90` (free-form). Legacy files are `.F` (fixed-form, 132-char lines, uppercase extension). File names: `<subroutine_name>.F90` for subroutines, `<module_name>_mod.F90` for modules.

### Required patterns

```fortran
module my_feature_mod
  implicit none
contains
  subroutine my_feature(arg1, arg2_size, arg2)
    use precision_mod,  only : WP
    use my_alloc_mod,   only : my_alloc
    use spmd_mod,       only : spmd_send   ! not MPI_Send directly
    implicit none
    integer,       intent(in)    :: arg2_size
    real(kind=WP), intent(inout) :: arg2(arg2_size)  ! explicit size, not arg2(:) or arg2(*)
    ! ...
  end subroutine my_feature
end module my_feature_mod
```

### Key rules (from `.github/copilot-instructions.md` and `template/template.F90`)

| Do | Don't |
|----|-------|
| Use `real(kind=WP)` | Use `DOUBLE PRECISION` or bare `REAL` |
| Indent 2 spaces | Use tabs |
| `ALLOCATABLE` arrays with `my_alloc` | Large automatic arrays or raw `allocate` |
| Explicit array sizes: `A(LEN)` | Assumed-size `A(*)` or assumed-shape `A(:)` in dummy args |
| Bounds for array ops: loop with indices | Whole-array `A = B + C` without bounds |
| `INTENT(IN/OUT/INOUT)` on all dummy args | Omit INTENT |
| Use modules with `ONLY` clause | `COMMON`, `EQUIVALENCE`, `SAVE`, `GOTO` |
| Keep leaf routines ≤ 200 lines | Runtime polymorphism, type-bound procedures |
| Use `my_alloc` and check allocation status | Rely on automatic deallocation |
| Deallocate as soon as possible | Leave arrays allocated unnecessarily |

### Performance rules

- Include `#include <vectorize.inc>` (IVDEP directive) in hot loops
- Work on arrays of size `MVSIZ` when possible
- Avoid `IF/THEN/ELSE`, `EXIT`, `CYCLE`, and subroutine calls inside performance-critical `DO` loops
- 2D array layout: if largest dim ≥ `MVSIZ`, put it last (`X(3,NUMNOD)`); if ≤ `MVSIZ`, put it first (`C(MVSIZ,5)`)
- Use integer exponents: `A**2` not `A**2.0`
- Never use non-contiguous data pointers (performance issue); prefer `ALLOCATABLE`

### Copyright header

Every source file must begin with the standard copyright block (see `template/template.F90`). The `scripts/headers.py` script updates headers automatically.

## Contribution Workflow

The project uses a fork-and-PR model. Work on feature branches derived from `main`, squash commits before opening a PR (`git rebase -i main`), and rebase on upstream before pushing:

```bash
git pull --rebase upstream main
git push -f origin <feature-branch>
```

A pre-push hook (`scripts/pre-push`) warns if the last commit author differs from the pusher (guards against accidental squashing of others' commits).

## CI

- **`developer_ci.yml`** — runs on every PR: builds starter + engine (dp and sp), runs code-analysis with a GCC plugin that checks Fortran signatures, and runs the miniqa test suite
- **`prmerge_ci_main.yml`** — runs on merge to `main`: full build matrix (Linux x86-64, Windows, ARM64, dp+sp) + full QA suite
- Code analysis uses a custom GCC plugin at `scripts/gcc_plugin/` (build with `scripts/gcc_plugin/build.sh`)
- Indentation of new `.F`/`.F90` files is checked via `scripts/check_indentation.sh` (uses `wfindent -i2`)

## Stable Releases

Stable releases are tagged `latest-YYYYMMDD` on the `main` branch. To switch to the latest stable:

```bash
git fetch --tags upstream main
git checkout $(git tag --sort=-version:refname | head -1)
```
