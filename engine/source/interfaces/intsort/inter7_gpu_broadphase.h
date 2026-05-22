/*Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.*/

/*
 * inter7_gpu_broadphase.h
 *
 * GPU-accelerated broad-phase collision detection for Interface Type 7.
 *
 * Architecture overview
 * =====================
 * The CPU algorithm (inter7_candidate_pairs.F90 / i7trivox.F) uses a uniform
 * voxel grid:
 *   Phase 1 – fill_voxel: assign each slave node to a voxel cell, build a
 *             per-cell singly-linked list (voxel[] head, next_nod[] chain).
 *   Phase 2 – candidate pairs: for every master segment find the voxel range
 *             that its inflated AABB covers, walk each cell's node list,
 *             and apply a set of rejection filters.
 *
 * The GPU implementation replaces Phase 2 (and optionally Phase 1) with CUDA
 * kernels.  It uses NVIDIA PhysX solely for GPU context management
 * (PxCudaContextManager) so that CUDA device memory and streams are managed
 * consistently with a simulation that already uses PhysX for other purposes.
 * The broad-phase logic itself is a custom sort-based algorithm that maps
 * exactly to the existing voxel data structure.
 *
 * Design decisions
 * ================
 * Master-surface vs slave-node asymmetry
 *   In the CPU code only slave nodes are inserted into voxels; master segments
 *   are never inserted.  During the query phase only (segment, node) pairs are
 *   produced, never (node, node) or (segment, segment).  The GPU preserves
 *   this: slave nodes are uploaded as a sorted array keyed by voxel index,
 *   master segments are each handled by one CUDA block.  Asymmetric pair
 *   generation is intrinsic to the algorithm – no explicit filter group trick
 *   is needed (though the PhysX AABBManager filter-group mechanism would work
 *   equally well if a full PhysX scene is available).
 *
 * Quick rejection of forbidden pairs
 *   Three levels, cheapest first:
 *   (a) Corner self-pair  – if the slave global node ID matches any of the
 *       four corner IDs (M1..M4) of the queried segment the pair is dropped.
 *       Cost: 4 integer comparisons in registers.
 *   (b) KREMNODE forbidden pair – the KREMNODE/REMNOD arrays encode, for each
 *       segment, a list of neighbouring nodes that must not contact it.  The
 *       GPU receives these lists in CSR format; an in-register binary search
 *       over the (typically short) per-segment list rejects forbidden pairs
 *       without extra memory traffic.
 *   (c) Plane distance underestimation – identical to the CPU formula:
 *       project the candidate node onto the segment normal and compare with
 *       AAA^2 * |normal|^2.  Applied after (a) and (b) pass.
 *
 * Calling convention
 * ==================
 * All entry points follow the Fortran ISO_C_BINDING calling convention
 * (trailing underscore, pass-by-reference for arrays, pass-by-value for
 * scalars where annotated).  The Fortran wrapper module
 * INTER7_GPU_BROADPHASE_MOD (inter7_gpu_broadphase_f.F90) provides
 * type-safe Fortran interfaces.
 *
 * Precision
 * =========
 * The real type matches OpenRadioss's WP (4 or 8 bytes).  Compile with
 *   -DMYREAL8   to use double (WP=8, default recommended for GPU compute)
 *   (no flag)   to use single (WP=4)
 *
 * Dependencies
 * ============
 *   CUDA Toolkit >= 11.0
 *   NVIDIA PhysX SDK >= 5.1  (only PxCudaContextManager is required)
 *   Thrust (bundled with CUDA Toolkit)
 */

#pragma once

#include <stdint.h>

#ifdef MYREAL8
  typedef double   my_real_gpu;
#else
  typedef float    my_real_gpu;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* -------------------------------------------------------------------------
 * Lifecycle
 * -------------------------------------------------------------------------
 * inter7_gpu_broadphase_init_
 *   Initialise the CUDA device and allocate persistent GPU resources.
 *   Must be called once before any broadphase query.
 *   Returns 0 on success, non-zero if no CUDA-capable device is available.
 *
 * inter7_gpu_broadphase_destroy_
 *   Release all GPU resources.  Call once at simulation end.
 * ------------------------------------------------------------------------- */
int  inter7_gpu_broadphase_init_(void);
void inter7_gpu_broadphase_destroy_(void);

/* -------------------------------------------------------------------------
 * inter7_gpu_fill_voxel_
 *
 * GPU replacement for fill_voxel_local (fill_voxel.F90).
 * Inserts slave nodes into the uniform voxel grid on the device.
 * The resulting voxel[] and next_nod[] arrays (device memory, managed
 * internally) are consumed by the subsequent inter7_gpu_candidate_pairs_
 * call within the same timestep.
 *
 * Parameters match fill_voxel_local arguments (see fill_voxel.F90).
 * All pointer arguments point to HOST (Fortran) memory; the function
 * uploads the required data to the device.
 * ------------------------------------------------------------------------- */
void inter7_gpu_fill_voxel_(
    const int*       nsn,          /* number of local slave nodes            */
    const int*       nsnr,         /* number of remote (SPMD) slave nodes    */
    const int*       nbx,          /* voxel grid dimensions                  */
    const int*       nby,
    const int*       nbz,
    const int*       nrtm,         /* number of master segments              */
    const int*       numnod,       /* total node count                       */
    const int*       nsv,          /* slave-to-global node map [nsn]         */
    const my_real_gpu* x,          /* node coords [3*numnod], col-major      */
    const my_real_gpu* stfn,       /* slave node stiffness [nsn]             */
    const my_real_gpu* box_limit   /* bounding box [12]                      */
);

/* -------------------------------------------------------------------------
 * inter7_gpu_candidate_pairs_
 *
 * GPU replacement for INTER7_CANDIDATE_PAIRS (inter7_candidate_pairs.F90).
 * Assumes the voxel grid has already been filled (either via
 * inter7_gpu_fill_voxel_ or the CPU fill_voxel routines – the voxel and
 * next_nod arrays passed here are the same ones used by the CPU path).
 *
 * The function uploads the voxel/next_nod state from host to device,
 * launches the GPU candidate-detection kernel, applies rejection filters,
 * and writes results back to the host cand_n/cand_e arrays.
 *
 * Parameters mirror INTER7_CANDIDATE_PAIRS (inter7_candidate_pairs.F90).
 * All pointer arguments point to HOST memory.
 * ------------------------------------------------------------------------- */
void inter7_gpu_candidate_pairs_(
    /* ---- geometry ---- */
    const int*       nsn,          /* number of slave nodes                  */
    const int*       nrtm,         /* number of master segments              */
    const int*       numnod,       /* total number of nodes                  */
    const int*       nsv,          /* slave-to-global node map [nsn]         */
    const int*       irect,        /* segment connectivity [4*nrtm], col-maj */
    const my_real_gpu* x,          /* node coords [3*numnod], col-major      */
    const my_real_gpu* stf,        /* segment stiffness [nrtm]               */
    const my_real_gpu* xyzm,       /* bounding boxes [12]: xmin,ymin,zmin,   */
                                   /*   xmax,ymax,zmax,xminb,...,zmaxb       */
    /* ---- voxel structure (host arrays, already filled) ---- */
    const int*       voxel,        /* voxel head array [(nbx+2)*(nby+2)*(nbz+2)] */
    const int*       next_nod,     /* next-node chain [nsn+nsnr]             */
    const int*       nsnr,         /* number of remote slave nodes           */
    const int*       nbx,          /* voxel grid dimensions                  */
    const int*       nby,
    const int*       nbz,
    /* ---- gap / contact parameters ---- */
    const int*       igap,         /* gap model flag                         */
    const my_real_gpu* marge,      /* margin = tzinf - max(gap+dgapload,drad)*/
    const my_real_gpu* tzinf,      /* zone-of-influence size                 */
    const my_real_gpu* gap,        /* scalar gap                             */
    const my_real_gpu* gapmin,
    const my_real_gpu* gapmax,
    const my_real_gpu* bgapsmx,    /* overestimation of gap_s                */
    const my_real_gpu* drad,       /* radiation distance (thermal)           */
    const my_real_gpu* dgapload,   /* gap load                               */
    const my_real_gpu* gap_s,      /* per-slave-node gap [nsn]               */
    const my_real_gpu* gap_m,      /* per-segment gap [nrtm]                 */
    const my_real_gpu* curv_max,   /* max curvature per segment [nrtm]       */
    /* ---- forbidden pair lists (KREMNODE) ---- */
    const int*       flagremnode,  /* 2 = kremnode active                    */
    const int*       s_kremnod,    /* size of kremnod array                  */
    const int*       kremnod,      /* kremnod index pairs [s_kremnod]        */
    const int*       s_remnod,     /* size of remnod array                   */
    const int*       remnod,       /* forbidden global node IDs [s_remnod]   */
    /* ---- output ---- */
    const int*       mulnsn,       /* maximum number of candidates           */
    int*             cand_n,       /* OUT: slave node local indices [mulnsn] */
    int*             cand_e,       /* OUT: segment local indices [mulnsn]    */
    int*             ii_stok       /* OUT: number of candidates found        */
);

/* -------------------------------------------------------------------------
 * inter7_gpu_available_
 *
 * Returns 1 if a CUDA-capable device has been successfully initialised,
 * 0 otherwise.  Fortran callers use this to decide whether to use the GPU
 * or fall back to the CPU path.
 * ------------------------------------------------------------------------- */
int inter7_gpu_available_(void);

#ifdef __cplusplus
}
#endif
