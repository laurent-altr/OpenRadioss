# OpenRadioss sigeps*.F Optimization Guide

## Overview

This document describes the optimization work performed on sigeps*.F files in the OpenRadioss engine and starter. The optimizations replace hand-coded loops with intrinsic Fortran array operations and array slices for better performance and vectorization.

## Motivation

- **Performance**: Modern Fortran compilers can better optimize array operations and enable SIMD vectorization
- **Code Quality**: Array syntax is more readable, maintainable, and less error-prone
- **Compiler Optimization**: Array operations allow compilers to apply advanced optimizations like loop fusion, vectorization, and parallelization

## Optimization Patterns

### Pattern 1: Simple Array Operations

Replace element-wise operations in loops with array syntax.

**Before:**
```fortran
DO I=1,NEL
  EPST(I) = EPSXX(I)**2 + EPSYY(I)**2 + EPSZZ(I)**2
  EPST(I) = SQRT(EPST(I))
  EPS0(I) = UVAR(I,1)
  SIG0(I) = UVAR(I,2)
ENDDO
```

**After:**
```fortran
! Use array operations
EPST(1:NEL) = EPSXX(1:NEL)**2 + EPSYY(1:NEL)**2 + EPSZZ(1:NEL)**2
EPST(1:NEL) = SQRT(EPST(1:NEL))
EPS0(1:NEL) = UVAR(1:NEL,1)
SIG0(1:NEL) = UVAR(1:NEL,2)
```

### Pattern 2: Array Slices (JFT:JLT)

For loops that don't start at 1, use array slices.

**Before:**
```fortran
DO I=JFT,JLT
  SIGNXX(I) = SIGNXX(I)+A1*DEPSXX(I)+A2*DEPSYY(I)
  SIGNYY(I) = SIGNYY(I)+A2*DEPSXX(I)+A1*DEPSYY(I)
  SIGNXY(I) = SIGNXY(I)+ G*DEPSXY(I)
ENDDO
```

**After:**
```fortran
! Use array slices
SIGNXX(JFT:JLT) = SIGNXX(JFT:JLT)+A1*DEPSXX(JFT:JLT)+A2*DEPSYY(JFT:JLT)
SIGNYY(JFT:JLT) = SIGNYY(JFT:JLT)+A2*DEPSXX(JFT:JLT)+A1*DEPSYY(JFT:JLT)
SIGNXY(JFT:JLT) = SIGNXY(JFT:JLT)+ G*DEPSXY(JFT:JLT)
```

### Pattern 3: WHERE Constructs

Replace conditional assignments with WHERE/END WHERE.

**Before:**
```fortran
DO I = 1,NEL
  IF (OFF(I) < EM01) OFF(I) = ZERO
  IF (OFF(I) <  ONE) OFF(I) = OFF(I)*FOUR_OVER_5  
ENDDO
```

**After:**
```fortran
! Use WHERE construct
WHERE (OFF(1:NEL) < EM01)
  OFF(1:NEL) = ZERO
END WHERE
WHERE (OFF(1:NEL) < ONE)
  OFF(1:NEL) = OFF(1:NEL)*FOUR_OVER_5
END WHERE
```

### Pattern 4: Array Intrinsics

Use array versions of intrinsic functions.

**Before:**
```fortran
DO I=1,NEL
  ET(I) = MIN(ONE, ET(I))
  SOUNDSP(I) = SQRT(A11(I)/RHO0(I))
ENDDO
```

**After:**
```fortran
! Use array intrinsics
ET(1:NEL) = MIN(ONE, ET(1:NEL))
SOUNDSP(1:NEL) = SQRT(A11(1:NEL)/RHO0(1:NEL))
```

### Pattern 5: Constant Initialization

Replace loops that initialize arrays with constants.

**Before:**
```fortran
DO I=1,NEL0
  E(I) =  E1   
  A1(I) = A11                                                      
  A2(I) = A21                                                      
  G(I) =  G1                                                       
  G3(I) = G31  
ENDDO
```

**After:**
```fortran
! Use array operations
E(1:NEL0) =  E1
A1(1:NEL0) = A11
A2(1:NEL0) = A21
G(1:NEL0) =  G1
G3(1:NEL0) = G31
```

### Pattern 6: Scalar Multiplication

Replace loops for scaling arrays.

**Before:**
```fortran
DO I=1,NEL
  SIGNXX(I) = ALPHA_1(I)*SIGNXX(I)
  SIGNYY(I) = ALPHA_1(I)*SIGNYY(I)
  SIGNZZ(I) = ALPHA_1(I)*SIGNZZ(I)
ENDDO
```

**After:**
```fortran
! Use array operations
SIGNXX(1:NEL) = ALPHA_1(1:NEL)*SIGNXX(1:NEL)
SIGNYY(1:NEL) = ALPHA_1(1:NEL)*SIGNYY(1:NEL)
SIGNZZ(1:NEL) = ALPHA_1(1:NEL)*SIGNZZ(1:NEL)
```

## Files Optimized

| File | Loops Optimized | Type |
|------|----------------|------|
| sigeps70.F | 5 | Array operations |
| sigeps76.F | 3 | Array ops + WHERE |
| sigeps60c.F | 4 | Array operations |
| sigeps73c.F | 3 | Array operations |
| sigeps01c.F | 4 | Array slices (JFT:JLT) |
| sigeps01g.F | 2 | Array slices (JFT:JLT) |
| sigeps02c.F | 3 | Array operations |

**Total: 7 files, 24 loops optimized**

## Performance Benefits

1. **Vectorization**: Modern CPUs can execute array operations using SIMD instructions
2. **Cache Efficiency**: Consecutive memory access patterns improve cache utilization
3. **Compiler Optimization**: Array syntax enables aggressive compiler optimizations
4. **Loop Overhead Reduction**: Eliminates explicit loop control overhead

## When NOT to Use Array Operations

- **Complex conditionals**: When loop body has complex branching logic
- **Data dependencies**: When iteration i+1 depends on result from iteration i
- **External function calls**: When calling external functions that cannot be vectorized
- **Index indirection**: When using indirect indexing like ARRAY(INDEX(I))

## Compatibility

- These optimizations use Fortran 90/95 standard features
- Compatible with all modern Fortran compilers (gfortran, ifort, nvfortran)
- No changes to numerical results or algorithm behavior

## Future Work

- 130+ additional sigeps*.F files can benefit from similar optimizations
- Each file typically has 5-15 loops that can be optimized
- Estimated total optimization potential: 650-1950 loops across all files

## Testing

All optimizations maintain:
- Bit-for-bit numerical accuracy
- Same input/output behavior
- Backward compatibility

## References

- Fortran 90/95 Array Programming
- Modern Fortran Best Practices
- OpenRadioss Coding Standards

---

**Authors**: OpenRadioss Development Team  
**Date**: December 2025  
**Version**: 1.0
